{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Article where

import RIO
import Rel8
import Hasql.Connection
import Data.Functor.Contravariant
import Data.List (head, (\\))
import Control.Monad.Trans.Maybe

import Conduit.Core.User
import Conduit.Core.Article
import Conduit.Db.Schema.User
import Conduit.Db.Schema.Article
import Conduit.Db.Schema.Tag
import Conduit.Db.Schema.ArticleTag
import Conduit.Db.Schema.Favorite
import Conduit.Db.Schema.UserFollow
import Conduit.App
import Conduit.Util

data ArticleFilters = ArticleFilters
    { getTagFilter      :: Maybe TagId
    , getFavoriteFilter :: Maybe UserId
    , getAuthorFilter   :: Maybe UserId
    }

data Pagination = Pagination
    { pageNum  :: Int64
    , pageSize :: Int64
    }

data EnrichedArticle = EnrichedArticle
    { enrArticle         :: Article
    , enrAuthor          :: User
    , enrFollowingAuthor :: Bool
    , enrIsFavorited     :: Bool
    , enrFavoritedCount  :: Int64
    }

type EnrichedArticleQuery = Query (MaybeTable (ListTable (TagEntity Expr)), UserEntity Expr, Expr Bool, Expr Bool, Expr Int64)

type EnrichedArticleResult = (Maybe [TagEntity Result], UserEntity Result, Bool, Bool, Int64)

mapArticleEntityWithTagsToArticle :: (ArticleEntity Result, [TagEntity Result]) -> Article
mapArticleEntityWithTagsToArticle (article, tags) =
    let article' = mapArticleEntityToArticle article
    in article' { articleTags = map entityTagText tags}

enrichedArticle :: (ArticleEntity Result, EnrichedArticleResult)
                                  -> EnrichedArticle
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
        tag'      = getTagFilter filters
        favorite' = getFavoriteFilter filters
        author'   = getAuthorFilter filters

getPagedArticleStmt :: Int64 -> Int64 -> ArticleFilters -> Query (ArticleEntity Expr)
getPagedArticleStmt pageSize page filters =
    limit (fromIntegral pageSize)
        $ offset (fromIntegral page)
        $ orderBy (entityArticleCreatedAt >$< desc)
        $ getAllArticleStmt filters

getTagsOfArticleStmt :: ArticleEntity Expr -> Query (MaybeTable (ListTable (TagEntity Expr)))
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

createArticle' :: forall m . MonadIO m => Connection -> Article -> m (Maybe Article)
createArticle' conn article = runMaybeT $ do
    articleId' <- MaybeT $ liftIO $ listToMaybe <$> insert conn (insertArticleStmt article)
    tagIds <- mapM (getOrCreateTagId' conn) (articleTags article)
    _ <- liftIO $ insert conn $ insertArticleTagsStmt articleId' tagIds
    return $ article { articleId = articleId' }

getTagId' :: forall m . MonadIO m => Connection -> Text -> m (Maybe TagId)
getTagId' conn tag = liftIO $ listToMaybe <$> select conn (getTagIdStmt tag)

getOrCreateTagId' :: forall m . MonadIO m => Connection -> Text -> m TagId
getOrCreateTagId' conn tag = do
    tagId <- getTagId' conn tag
    case tagId of
        Just tagId' -> return tagId'
        Nothing -> liftIO $ head <$> insert conn (insertTagStmt tag)

-- cleanTags :: forall m . MonadIO m => Connection -> [TagId] -> m Int64
-- cleanTags conn tagIds = liftIO $ delete

getEnrichedArticleById' :: forall m . MonadIO m => Connection -> Maybe User -> ArticleId -> m (Maybe EnrichedArticle)
getEnrichedArticleById' conn mbUser id = do
    records <- liftIO $ select conn $ do
        article <- getArticleEntityByIdStmt $ litExpr id
        enrichedData <- getArticleEnrichedDataStmt mbUser article
        return (article, enrichedData)
    return $ listToMaybe $ map enrichedArticle records

getArticleById' :: forall m . MonadIO m => Connection -> ArticleId -> m (Maybe Article)
getArticleById' conn id = do
    records <- liftIO $ listToMaybe <$> select conn (getArticleEntityByIdStmt $ litExpr id)
    return $ mapArticleEntityToArticle <$> records

getEnrichedArticleBySlug' :: forall m . MonadIO m => Connection -> Maybe User -> Slug -> m (Maybe EnrichedArticle)
getEnrichedArticleBySlug' conn mbUser slug = do
    records <- liftIO $ select conn $ do
        article <- getArticleEntityBySlugStmt $ litExpr slug
        enrichedData <- getArticleEnrichedDataStmt mbUser article
        return (article, enrichedData)
    return $ listToMaybe $ map enrichedArticle records

getArticleBySlug' :: forall m . MonadIO m => Connection -> Slug -> m (Maybe Article)
getArticleBySlug' conn slug = do
    records <- liftIO $ listToMaybe <$> select conn (getArticleEntityBySlugStmt $ litExpr slug)
    return $ mapArticleEntityToArticle <$> records

getPagedArticle' :: forall m . MonadIO m => Connection -> Maybe User -> Pagination -> ArticleFilters -> m ([EnrichedArticle], Int64)
getPagedArticle' conn mbUser p filters = do
    total <- liftIO $ select conn $ aggregate $ do
        _ <- getAllArticleStmt filters
        return countStar
    pagedResults <- liftIO $ select conn $ do
        article <- getPagedArticleStmt (pageSize p) (pageNum p) filters
        enrichedData <- getArticleEnrichedDataStmt mbUser article
        return (article, enrichedData)
    return (map enrichedArticle pagedResults, fromMaybe 0 . listToMaybe $ total)

checkFavorite' :: forall m . MonadIO m => Connection -> User -> ArticleId  -> m Bool
checkFavorite' conn user articleId = do
    exists <- liftIO $ select conn $ checkFavoriteStmt (userId user) (litExpr articleId)
    return $ exists == [True]

addFavorite' :: MonadIO m => Connection -> User -> ArticleId -> m Bool
addFavorite' conn user articleId = do
    rowsEffected <- liftIO $ insert conn $ addFavoritedArticleStmt (userId user) articleId
    return $ rowsEffected == 1

removeFavorite' :: MonadIO m => Connection -> User -> ArticleId -> m Bool
removeFavorite' conn user articleId = do
    rowsEffected <- liftIO $ delete conn $ removeFavoritedArticleStmt (userId user) articleId
    return $ rowsEffected == 1

deleteArticleById' :: MonadIO m => Connection -> ArticleId -> m Bool
deleteArticleById' conn articleId = do
    _ <- liftIO $ delete conn $ deleteAllArticleTagsStmt articleId
    _ <- liftIO $ delete conn $ removeAllFavoritesByArticleIdStmt articleId
    rowsEffected <- liftIO $ delete conn $ deleteArticleByIdStmt articleId
    return $ rowsEffected == 1

deleteArticleTag' :: forall m . MonadIO m => Connection -> ArticleId -> TagId -> m Int64
deleteArticleTag' conn articleId tagId = liftIO . delete conn $ deleteArticleTagStmt articleId tagId

updateArticle' :: MonadIO m => Connection -> Article -> m Bool
updateArticle' conn article = do
    currentTagIds <- mapM (getOrCreateTagId' conn) (articleTags article)
    allLinkedTagIds <- liftIO $ select conn $ getAllArticleTagsStmt (litExpr $ articleId article)
    mapM_ (deleteArticleTag' conn (articleId article)) $ allLinkedTagIds \\ currentTagIds
    liftIO $ insert conn $ insertArticleTagsStmt (articleId article) currentTagIds
    rowsEffected <- liftIO $ update conn $ updateArticleStmt article
    return $ rowsEffected == 1

getArticleFavoritedCount' :: MonadIO m => Connection -> ArticleId -> m Int64
getArticleFavoritedCount' conn articleId = do
    count <- liftIO $ select conn $ getFavoritedCountOfArticleStmt (litExpr articleId)
    return $ fromMaybe 0 . listToMaybe $ count

{--------------------------------------------------------------------------------------------------------------------}

createArticle :: Article -> AppM (Maybe Article)
createArticle = flipM getConn createArticle'

getArticleById :: ArticleId -> AppM (Maybe Article)
getArticleById = flipM getConn getArticleById'

getArticleBySlug :: Slug -> AppM (Maybe Article)
getArticleBySlug = flipM getConn getArticleBySlug'

getEnrichedArticleById :: Maybe User -> ArticleId -> AppM (Maybe EnrichedArticle)
getEnrichedArticleById = flipM2 getConn getEnrichedArticleById'

getEnrichedArticleBySlug :: Maybe User -> Slug -> AppM (Maybe EnrichedArticle)
getEnrichedArticleBySlug = flipM2 getConn getEnrichedArticleBySlug'

checkFavorite :: User -> ArticleId -> AppM Bool
checkFavorite = flipM2 getConn checkFavorite'

addFavorite :: User -> ArticleId -> AppM Bool
addFavorite = flipM2 getConn addFavorite'

removeFavorite :: User -> ArticleId -> AppM Bool
removeFavorite = flipM2 getConn removeFavorite'

getPagedArticle :: Maybe User -> Pagination -> ArticleFilters -> AppM ([EnrichedArticle], Int64)
getPagedArticle = flipM3 getConn getPagedArticle'

getTagId :: Text -> AppM (Maybe TagId)
getTagId = flipM getConn getTagId'

deleteArticleById :: ArticleId -> AppM Bool
deleteArticleById = flipM getConn deleteArticleById'

updateArticle :: Article -> AppM Bool
updateArticle = flipM getConn updateArticle'

getArticleFavoritedCount :: ArticleId -> AppM Int64
getArticleFavoritedCount = flipM getConn getArticleFavoritedCount'
