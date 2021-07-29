{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Article where

import RIO
import Rel8
import Hasql.Connection
import Data.Functor.Contravariant
import Data.List (head)
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
    articleTag <- each articleTagSchema
    where_ $ entityArticleId article ==. tgdArticleId articleTag
    tag <- each tagSchema
    where_ $ entityTagId tag ==. tgdTagId articleTag
    return $ listAgg tag

getArticleEnrichedDataStmt :: Maybe User -> ArticleEntity Expr -> EnrichedArticleQuery
getArticleEnrichedDataStmt mbUser article = do
    tags <- getTagsOfArticleStmt article
    favoritedCount <- countRows $ do
        favorite <- each favoriteSchema
        where_ $ entityArticleId article ==. favoriteArticleId favorite
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

getCompletedArticleById' :: forall m . MonadIO m => Connection -> Maybe User -> ArticleId -> m (Maybe EnrichedArticle)
getCompletedArticleById' conn mbUser id = do
    records <- liftIO $ select conn $ do
        article <- getArticleEntityByIdStmt id
        enrichedData <- getArticleEnrichedDataStmt mbUser article
        return (article, enrichedData)
    return $ listToMaybe $ map enrichedArticle records

getArticleById' :: forall m . MonadIO m => Connection -> ArticleId -> m (Maybe Article)
getArticleById' conn id = do
    records <- liftIO $ listToMaybe <$> select conn (getArticleEntityByIdStmt id)
    return $ mapArticleEntityToArticle <$> records

getCompletedArticleBySlug' :: forall m . MonadIO m => Connection -> Maybe User -> Slug -> m (Maybe EnrichedArticle)
getCompletedArticleBySlug' conn mbUser slug = do
    records <- liftIO $ select conn $ do
        article <- getArticleEntityBySlugStmt slug
        enrichedData <- getArticleEnrichedDataStmt mbUser article
        return (article, enrichedData)
    return $ listToMaybe $ map enrichedArticle records

getArticleBySlug' :: forall m . MonadIO m => Connection -> Username -> Slug -> m (Maybe Article)
getArticleBySlug' conn author slug = do
    records <- liftIO $ listToMaybe <$> select conn (getArticleEntityBySlugStmt slug)
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

{--------------------------------------------------------------------------------------------------------------------}

createArticle :: Article -> AppM (Maybe Article)
createArticle = flipM getConn createArticle'

getCompletedArticleById :: Maybe User -> ArticleId -> AppM (Maybe EnrichedArticle)
getCompletedArticleById = flipM2 getConn getCompletedArticleById'

getCompletedArticleBySlug :: Maybe User -> Slug -> AppM (Maybe EnrichedArticle)
getCompletedArticleBySlug = flipM2 getConn getCompletedArticleBySlug'

checkFavorite :: User -> ArticleId -> AppM Bool
checkFavorite = flipM2 getConn checkFavorite'

getPagedArticle :: Maybe User -> Pagination -> ArticleFilters -> AppM ([EnrichedArticle], Int64)
getPagedArticle = flipM3 getConn getPagedArticle'

getTagId :: Text -> AppM (Maybe TagId)
getTagId = flipM getConn getTagId'
