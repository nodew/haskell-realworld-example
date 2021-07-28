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
    { afsTag :: Maybe TagId
    , afsFavorite :: Maybe UserId
    , afsAuthor :: Maybe UserId
    }

data Pagination = Pagination
    { pageNum  :: Int64
    , pageSize :: Int64
    }

mapArticleEntityWithTagsToArticle :: (ArticleEntity Result, [TagEntity Result]) -> Article
mapArticleEntityWithTagsToArticle (article, tags) =
    let article' = mapArticleEntityToArticle article
    in article' { articleTags = map tagText tags}

mapArticleWithContextData :: (ArticleEntity Result, Maybe [TagEntity Result], UserEntity Result, Bool, Bool, Int64)
                          -> (Article, User, Bool, Bool, Int64)
mapArticleWithContextData (article, tags, author, followingAuthor, favorited, favoritedCount) =
            ( mapArticleEntityWithTagsToArticle (article, fromMaybe [] tags)
            , mapUserEntityToUser author
            , followingAuthor
            , favorited
            , favoritedCount
            )

filterArticleByAuthorStmt :: ArticleEntity Expr -> UserId -> Expr Bool
filterArticleByAuthorStmt article uid = _articleAuthorId article ==. lit uid

filterArticleByFavoriteStmt :: ArticleEntity Expr -> UserId -> Query ()
filterArticleByFavoriteStmt article uid = do
    favorite <- each favoriteSchema
    where_ $ favoriteUserId favorite ==. lit uid &&. favoriteArticleId favorite ==. _articleId article

filterArticleByTagStmt :: ArticleEntity Expr -> TagId -> Query ()
filterArticleByTagStmt article tagId = do
    tag <- each articleTagSchema
    where_ $ tgdTagId tag ==. lit tagId &&. _articleId article ==. tgdArticleId tag

getAllArticleStmt :: ArticleFilters -> Query (ArticleEntity Expr)
getAllArticleStmt filters = do
    article <- each articleSchema
    where_ $ maybe (lit True) (filterArticleByAuthorStmt article) author'
    forM_ favorite' $ filterArticleByFavoriteStmt article
    forM_ tag' $ filterArticleByTagStmt article
    return article
    where
        tag' = afsTag filters
        favorite' = afsFavorite filters
        author' = afsAuthor filters

getLimitedArticleStmt :: Int64 -> Int64 -> ArticleFilters -> Query (ArticleEntity Expr)
getLimitedArticleStmt pageSize page filters =
    limit (fromIntegral pageSize)
        $ offset (fromIntegral page)
        $ orderBy (_articleCreatedAt >$< desc)
        $ getAllArticleStmt filters

getTagsOfArticleStmt :: ArticleEntity Expr -> Query (MaybeTable (ListTable (TagEntity Expr)))
getTagsOfArticleStmt article = Rel8.optional $ aggregate $ do
    articleTag <- each articleTagSchema
    where_ $ _articleId article ==. tgdArticleId articleTag
    tag <- each tagSchema
    where_ $ tagId tag ==. tgdTagId articleTag
    return $ listAgg tag

getArticleFeatureDataStmt :: ArticleEntity Expr -> Query (MaybeTable(ListTable (TagEntity Expr)), Expr Int64)
getArticleFeatureDataStmt article = do
    tags <- getTagsOfArticleStmt article
    favoritedCount <- countRows $ do
        favorite <- each favoriteSchema
        where_ $ _articleId article ==. favoriteArticleId favorite
    return (tags, favoritedCount)

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

getCompletedArticleById' :: forall m . MonadIO m => Connection -> Maybe User -> ArticleId -> m (Maybe (Article, User, Bool, Bool, Int64))
getCompletedArticleById' conn maybeUser id = do
    records <- liftIO $ select conn $ do
        article <- getArticleEntityByIdStmt id
        (tags, favoritedCount) <- getArticleFeatureDataStmt article
        author <- getUserByIdStmt (_articleAuthorId article)
        favorited <- maybe (return $ litExpr False) (\user -> checkFavoriteStmt (userId user) (_articleId article)) maybeUser
        followingAuthor <- maybe (return $ litExpr False) (\user -> checkFollowshipStmt user (_articleAuthorId article)) maybeUser
        return (article, tags, author, followingAuthor, favorited, favoritedCount)
    return $ listToMaybe $ map mapArticleWithContextData records

getArticleById' :: forall m . MonadIO m => Connection -> ArticleId -> m (Maybe Article)
getArticleById' conn id = do
    records <- liftIO $ listToMaybe <$> select conn (getArticleEntityByIdStmt id)
    return $ mapArticleEntityToArticle <$> records

getCompletedArticleBySlug' :: forall m . MonadIO m => Connection -> Maybe User -> Slug -> m (Maybe (Article, User, Bool, Bool, Int64))
getCompletedArticleBySlug' conn maybeUser slug = do
    records <- liftIO $ select conn $ do
        article <- getArticleEntityBySlugStmt slug
        (tags, favoritedCount) <- getArticleFeatureDataStmt article
        author <- getUserByIdStmt (_articleAuthorId article)
        favorited <- maybe (return $ litExpr False) (\user -> checkFavoriteStmt (userId user) (_articleId article)) maybeUser
        followingAuthor <- maybe (return $ litExpr False) (\user -> checkFollowshipStmt user (_articleAuthorId article)) maybeUser
        return (article, tags, author, followingAuthor, favorited, favoritedCount)
    return $ listToMaybe $ map mapArticleWithContextData records

getArticleBySlug' :: forall m . MonadIO m => Connection -> Username -> Slug -> m (Maybe Article)
getArticleBySlug' conn author slug = do
    records <- liftIO $ listToMaybe <$> select conn (getArticleEntityBySlugStmt slug)
    return $ mapArticleEntityToArticle <$> records

getAllArticle' :: forall m . MonadIO m => Connection -> Maybe User -> Pagination -> ArticleFilters -> m ([(Article, User, Bool, Bool, Int64)], Int64)
getAllArticle' conn maybeUser p filters = do
    total <- liftIO $ select conn $ aggregate $ do
        _ <- getAllArticleStmt filters
        return countStar
    pagedResults <- liftIO $ select conn $ do
        article <- getLimitedArticleStmt (pageSize p) (pageNum p) filters
        (tags, favoritedCount) <- getArticleFeatureDataStmt article
        author <- each userSchema
        where_ $ _userId author ==. _articleAuthorId article
        favorited <- maybe (return $ litExpr False) (\user -> checkFavoriteStmt (userId user) (_articleId article)) maybeUser
        followingAuthor <- maybe (return $ litExpr False) (\user -> checkFollowshipStmt user (_articleAuthorId article)) maybeUser
        return (article, tags, author, followingAuthor, favorited, favoritedCount)
    return (map mapArticleWithContextData pagedResults, fromMaybe 0 . listToMaybe $ total)

checkFavorite' :: forall m . MonadIO m => Connection -> User -> ArticleId  -> m Bool
checkFavorite' conn user articleId = do
    exists <- liftIO $ select conn $ checkFavoriteStmt (userId user) (litExpr articleId)
    return $ exists == [True]

{--------------------------------------------------------------------------------------------------------------------}

createArticle :: Article -> AppM (Maybe Article)
createArticle = flipM getConn createArticle'

getCompletedArticleById :: Maybe User -> ArticleId -> AppM (Maybe (Article, User, Bool, Bool, Int64))
getCompletedArticleById = flipM2 getConn getCompletedArticleById'

getCompletedArticleBySlug :: Maybe User -> Slug -> AppM (Maybe (Article, User, Bool, Bool, Int64))
getCompletedArticleBySlug = flipM2 getConn getCompletedArticleBySlug'

checkFavorite :: User -> ArticleId -> AppM Bool
checkFavorite = flipM2 getConn checkFavorite'

getAllArticle :: Maybe User -> Pagination -> ArticleFilters -> AppM ([(Article, User, Bool, Bool, Int64)], Int64)
getAllArticle = flipM3 getConn getAllArticle'

getTagId :: Text -> AppM (Maybe TagId)
getTagId = flipM getConn getTagId'
