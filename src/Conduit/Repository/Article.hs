{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Conduit.Repository.Article where

import Conduit.App
import Conduit.Core.Article
import Conduit.Core.User
import Conduit.Db
import Conduit.Util
import Data.Functor.Contravariant
import Data.List (head, (\\))
import Hasql.Transaction (Transaction)
import RIO
import Rel8

data ArticleFilters = ArticleFilters
    { getTagFilter :: Maybe TagId
    , getFavoriteFilter :: Maybe UserId
    , getAuthorFilter :: Maybe UserId
    }

data Pagination = Pagination
    { pageNum :: Int64
    , pageSize :: Int64
    }

data EnrichedArticle = EnrichedArticle
    { enrArticle :: Article
    , enrAuthor :: User
    , enrFollowingAuthor :: Bool
    , enrIsFavorited :: Bool
    , enrFavoritedCount :: Int64
    }

type EnrichedArticleQuery = Query (MaybeTable Expr (ListTable Expr (TagEntity Expr)), UserEntity Expr, Expr Bool, Expr Bool, Expr Int64)

type EnrichedArticleResult = (Maybe [TagEntity Result], UserEntity Result, Bool, Bool, Int64)

mapArticleEntityWithTagsToArticle :: (ArticleEntity Result, [TagEntity Result]) -> Article
mapArticleEntityWithTagsToArticle (article, tags) =
    let article' = mapArticleEntityToArticle article
     in article' {articleTags = map entityTagText tags}

enrichedArticle ::
    (ArticleEntity Result, EnrichedArticleResult) ->
    EnrichedArticle
enrichedArticle (articleEntity, (mbTags, authorEntity, followingAuthor, favorited, favoritedCount)) =
    EnrichedArticle
        (mapArticleEntityWithTagsToArticle (articleEntity, fromMaybe [] mbTags))
        (mapUserEntityToUser authorEntity)
        followingAuthor
        favorited
        favoritedCount

filterArticleByAuthorStmt :: ArticleEntity Expr -> UserId -> Expr Bool
filterArticleByAuthorStmt article uid = entityArticleAuthorId article ==. lit uid

filterArticleByFavoriteStmt :: ArticleEntity Expr -> UserId -> Query ()
filterArticleByFavoriteStmt article uid = do
    favorite <- each favoriteSchema
    where_ $ favoriteUserId favorite ==. lit uid &&. favoriteArticleId favorite ==. entityArticleId article

filterArticleByTagStmt :: ArticleEntity Expr -> TagId -> Query ()
filterArticleByTagStmt article tagId = do
    tag <- each articleTagSchema
    where_ $ tgdTagId tag ==. lit tagId &&. entityArticleId article ==. tgdArticleId tag

getAllArticleStmt :: ArticleFilters -> Query (ArticleEntity Expr)
getAllArticleStmt filters = do
    article <- each articleSchema
    where_ $ maybe (lit True) (filterArticleByAuthorStmt article) author'
    forM_ favorite' $ filterArticleByFavoriteStmt article
    forM_ tag' $ filterArticleByTagStmt article
    return article
    where
        tag' = getTagFilter filters
        favorite' = getFavoriteFilter filters
        author' = getAuthorFilter filters

getPagedArticleStmt :: Int64 -> Int64 -> ArticleFilters -> Query (ArticleEntity Expr)
getPagedArticleStmt pageSize page filters =
    limit (fromIntegral pageSize) $
        offset (fromIntegral page) $
            orderBy (entityArticleCreatedAt >$< desc) $
                getAllArticleStmt filters

getTagsOfArticleStmt ::
    (Column f ArticleId ~ Expr ArticleId) =>
    ArticleEntity f ->
    Query (MaybeTable Expr (ListTable Expr (TagEntity Expr)))
getTagsOfArticleStmt article = Rel8.optional $ aggregate $ do
    tagId <- getAllArticleTagsStmt (entityArticleId article)
    tag <- each tagSchema
    where_ $ entityTagId tag ==. tagId
    return $ listAgg tag

getArticleEnrichedDataStmt :: Maybe User -> ArticleEntity Expr -> EnrichedArticleQuery
getArticleEnrichedDataStmt mbUser article = do
    tags <- getTagsOfArticleStmt article
    favoritedCount <- getFavoritedCountOfArticleStmt (entityArticleId article)
    author <- getUserByIdStmt (entityArticleAuthorId article)
    favorited <- maybe (return $ litExpr False) (\user -> checkFavoriteStmt (userId user) (entityArticleId article)) mbUser
    followingAuthor <- maybe (return $ litExpr False) (\user -> checkFollowshipStmt user (entityArticleAuthorId article)) mbUser
    return (tags, author, followingAuthor, favorited, favoritedCount)

{--------------------------------------------------------------------------------------------------------------------}

createArticle :: Article -> AppM (Maybe Article)
createArticle article = runTransaction $ do
    mbArticleId <- runStmt $ listToMaybe <$> insert (insertArticleStmt article)
    case mbArticleId of
        Just articleId' -> do
            tagIds <- mapM getOrCreateTagId' (articleTags article)
            _ <- runStmt $ insert $ insertArticleTagsStmt articleId' tagIds
            return $ Just article {articleId = articleId'}
        Nothing -> return Nothing

getTagId' :: Text -> Transaction (Maybe TagId)
getTagId' tag = runStmt $ listToMaybe <$> select (getTagIdStmt tag)

getTagId :: Text -> AppM (Maybe TagId)
getTagId tag = runTransaction $ getTagId' tag

getOrCreateTagId' :: Text -> Transaction TagId
getOrCreateTagId' tag = do
    tagId <- getTagId' tag
    maybe
        (runStmt $ head <$> insert (insertTagStmt tag))
        return
        tagId

getEnrichedArticleById :: Maybe User -> ArticleId -> AppM (Maybe EnrichedArticle)
getEnrichedArticleById mbUser id = do
    records <- executeStmt $ select $ do
        article <- getArticleEntityByIdStmt $ litExpr id
        enrichedData <- getArticleEnrichedDataStmt mbUser article
        return (article, enrichedData)
    return $ listToMaybe $ map enrichedArticle records

getArticleById :: ArticleId -> AppM (Maybe Article)
getArticleById id = do
    records <- executeStmt $ listToMaybe <$> select (getArticleEntityByIdStmt $ litExpr id)
    return $ mapArticleEntityToArticle <$> records

getEnrichedArticleBySlug :: Maybe User -> Slug -> AppM (Maybe EnrichedArticle)
getEnrichedArticleBySlug mbUser slug = do
    records <- executeStmt $ select $ do
        article <- getArticleEntityBySlugStmt $ litExpr slug
        enrichedData <- getArticleEnrichedDataStmt mbUser article
        return (article, enrichedData)
    return $ listToMaybe $ map enrichedArticle records

getArticleBySlug :: Slug -> AppM (Maybe Article)
getArticleBySlug slug = do
    records <- executeStmt $ listToMaybe <$> select (getArticleEntityBySlugStmt $ litExpr slug)
    return $ mapArticleEntityToArticle <$> records

getPagedArticle :: Maybe User -> Pagination -> ArticleFilters -> AppM ([EnrichedArticle], Int64)
getPagedArticle mbUser p filters = do
    (total, pagedResults) <- runTransaction $ do
        total <- runStmt $ select $ aggregate $ do
            _ <- getAllArticleStmt filters
            return countStar
        pagedResults <- runStmt $ select $ do
            article <- getPagedArticleStmt (pageSize p) (pageNum p) filters
            enrichedData <- getArticleEnrichedDataStmt mbUser article
            return (article, enrichedData)
        return (total, pagedResults)
    return (map enrichedArticle pagedResults, fromMaybe 0 . listToMaybe $ total)

checkFavorite :: User -> ArticleId -> AppM Bool
checkFavorite user articleId = do
    exists <- executeStmt $ select $ checkFavoriteStmt (userId user) (litExpr articleId)
    return $ exists == [True]

addFavorite :: User -> ArticleId -> AppM Bool
addFavorite user articleId = do
    rowsEffected <- executeStmt $ insert $ addFavoritedArticleStmt (userId user) articleId
    return $ rowsEffected == 1

removeFavorite :: User -> ArticleId -> AppM Bool
removeFavorite user articleId = do
    rowsEffected <- executeStmt $ delete $ removeFavoritedArticleStmt (userId user) articleId
    return $ rowsEffected == 1

deleteArticleById :: ArticleId -> AppM Bool
deleteArticleById articleId = do
    rowsEffected <- runTransaction $ do
        _ <- runStmt $ delete $ deleteAllArticleTagsStmt articleId
        _ <- runStmt $ delete $ removeAllFavoritesByArticleIdStmt articleId
        runStmt $ delete $ deleteArticleByIdStmt articleId
    return $ rowsEffected == 1

deleteArticleTag' :: ArticleId -> TagId -> Transaction Int64
deleteArticleTag' articleId tagId = runStmt $ delete $ deleteArticleTagStmt articleId tagId

updateArticle :: Article -> AppM Bool
updateArticle article = do
    rowsEffected <- runTransaction $ do
        currentTagIds <- mapM getOrCreateTagId' (articleTags article)
        allLinkedTagIds <- runStmt $ select $ getAllArticleTagsStmt (litExpr $ articleId article)
        mapM_ (deleteArticleTag' (articleId article)) $ allLinkedTagIds \\ currentTagIds
        runStmt $ insert $ insertArticleTagsStmt (articleId article) currentTagIds
        runStmt $ update $ updateArticleStmt article
    return $ rowsEffected == 1

getArticleFavoritedCount :: ArticleId -> AppM Int64
getArticleFavoritedCount articleId = do
    count <- executeStmt $ select $ getFavoritedCountOfArticleStmt (litExpr articleId)
    return $ fromMaybe 0 . listToMaybe $ count
