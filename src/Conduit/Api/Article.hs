{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Conduit.Api.Article where

import RIO
import Data.Aeson
import Servant
import Data.Time
import Control.Monad.Trans.Maybe

import Conduit.App
import Conduit.Core.User
import Conduit.Core.Article
import qualified Conduit.Db.Article as ArticleDb
import qualified Conduit.Db.User as UserDb
import Conduit.Api.Common
import Conduit.Util

data NewArticleData = NewArticleData
    { newArticleTitle       :: Text
    , newArticleDescription :: Text
    , newArticleBody        :: Text
    , newArticleTagList     :: [Text]
    } deriving (Show, Generic)

instance FromJSON NewArticleData where
    parseJSON = genericParseJSON $ toJsonOptions 10

data UpdateArticleData = UpdateArticleData
    { updateArticleTitle       :: Maybe Text
    , updateArticleDescription :: Maybe Text
    , updateArticleBody        :: Maybe Text
    , updateArticleTagList     :: Maybe [Text]
    } deriving (Show, Generic)

instance FromJSON UpdateArticleData where
    parseJSON = genericParseJSON $ toJsonOptions 13

data ArticleData = ArticleData
    { articleDataSlug           :: Text
    , articleDataTitle          :: Text
    , articleDataDescription    :: Text
    , articleDataBody           :: Text
    , articleDataTagList        :: [Text]
    , articleDataCreatedAt      :: UTCTime
    , articleDataUpdatedAt      :: UTCTime
    , articleDataFavorited      :: Bool
    , articleDataFavoritesCount :: Int64
    , articleDataAuthor         :: UserProfile
    } deriving (Show, Generic)

instance ToJSON ArticleData where
    toJSON = genericToJSON $ toJsonOptions 11

newtype BoxedArticle a = BoxedArticle
    { boxedArticle :: a } deriving (Show, Generic)

instance ToJSON a => ToJSON (BoxedArticle a) where
    toJSON = genericToJSON $ toJsonOptions 5

instance FromJSON a => FromJSON (BoxedArticle a) where
    parseJSON = genericParseJSON $ toJsonOptions 5

data ArticlesResponse = ArticlesResponse
    { articlesRespArticlesCount :: Int64
    , articlesRespArticles      :: [ArticleData]
    } deriving (Show, Generic)

instance ToJSON ArticlesResponse where
    toJSON = genericToJSON $ toJsonOptions 12

mapArticleToArticleData :: Article -> UserProfile -> Bool -> Int64 -> ArticleData
mapArticleToArticleData article authorProfile favorited favoritedCount =
    ArticleData
        { articleDataSlug           = getSlug $ articleSlug article
        , articleDataTitle          = articleTitle article
        , articleDataDescription    = articleDescription article
        , articleDataBody           = articleBody article
        , articleDataTagList        = articleTags article
        , articleDataCreatedAt      = articleCreatedAt article
        , articleDataUpdatedAt      = articleUpdatedAt article
        , articleDataFavorited      = favorited
        , articleDataFavoritesCount = favoritedCount
        , articleDataAuthor         = authorProfile
        }

mapEnrichedArticleToArticleData :: ArticleDb.EnrichedArticle -> ArticleData
mapEnrichedArticleToArticleData (ArticleDb.EnrichedArticle article author followingAuthor favorited favoritedCount) =
    mapArticleToArticleData article (mapUserToUserProfile author followingAuthor) favorited favoritedCount

type ArticleApi = AuthProtect "Optional"
                        :> "articles"
                        :> QueryParam "page"      Int64
                        :> QueryParam "pageSize"  Int64
                        :> QueryParam "author"    Text
                        :> QueryParam "favorited" Text
                        :> QueryParam "tag"       Text
                        :> Get '[JSON] ArticlesResponse
                :<|> AuthProtect "Optional"
                        :> "articles"
                        :> Capture "slug" Slug
                        :> Get '[JSON] (BoxedArticle ArticleData)
                :<|> AuthProtect "Required"
                        :> "articles"
                        :> ReqBody '[JSON] (BoxedArticle NewArticleData)
                        :> Post '[JSON] (BoxedArticle ArticleData)
                :<|> AuthProtect "Required"
                        :> "articles"
                        :> Capture "slug" Slug
                        :> ReqBody '[JSON] (BoxedArticle UpdateArticleData)
                        :> Put '[JSON] (BoxedArticle ArticleData)
                :<|> AuthProtect "Required"
                        :> "articles"
                        :> Capture "slug" Slug
                        :> Delete '[JSON] NoContent
                :<|> AuthProtect "Required"
                        :> "articles"
                        :> Capture "slug" Slug
                        :> "favorite"
                        :> Post '[JSON] (BoxedArticle ArticleData)
                :<|> AuthProtect "Required"
                        :> "articles"
                        :> Capture "slug" Slug
                        :> "favorite"
                        :> Delete '[JSON] (BoxedArticle ArticleData)

getArticlesHandler :: Maybe User
                   -> Maybe Int64
                   -> Maybe Int64
                   -> Maybe Text
                   -> Maybe Text
                   -> Maybe Text
                   -> AppM ArticlesResponse
getArticlesHandler mbUser mbPage mbPageSize mbAuthorName mbFavorited mbTag = do
    (pagedResults, total) <- do
        tagId <- maybe (return Nothing) ArticleDb.getTagId mbTag
        withValidFilter mbTag tagId $ do
            author <- maybe (return Nothing) UserDb.getUserByName (Username <$> mbAuthorName)
            withValidFilter mbAuthorName author $ do
                favoritedBy <- maybe (return Nothing) UserDb.getUserByName (Username <$> mbFavorited)
                withValidFilter mbFavorited favoritedBy $ do
                    let filters = ArticleDb.ArticleFilters tagId (userId <$> favoritedBy) (userId <$> author)
                    let pageParam = ArticleDb.Pagination (fromMaybe 0 mbPage) (fromMaybe 20 mbPageSize)
                    ArticleDb.getPagedArticle mbUser pageParam filters

    return $ ArticlesResponse total $ map mapEnrichedArticleToArticleData pagedResults
    where
        isValidParam :: Maybe a -> Maybe b -> Bool
        isValidParam (Just _) Nothing = False
        isValidParam _        _       = True

        withValidFilter :: forall a b c . Maybe a -> Maybe b -> AppM ([c], Int64) -> AppM ([c], Int64)
        withValidFilter a b m
            | isValidParam a b = m
            | otherwise        = return ([], 0)

getArticleBySlugHandler :: Maybe User -> Slug -> AppM (BoxedArticle ArticleData)
getArticleBySlugHandler mbUser slug = do
    result <- ArticleDb.getEnrichedArticleBySlug mbUser slug
    case result of
        Nothing -> throwIO err404
        Just enrichedArticle ->
            return $ BoxedArticle $ mapEnrichedArticleToArticleData enrichedArticle

getArticleByIdHandler :: Maybe User -> ArticleId -> AppM (BoxedArticle ArticleData)
getArticleByIdHandler mbUser articleId = do
    result <- ArticleDb.getEnrichedArticleById mbUser articleId
    case result of
        Nothing -> throwIO err404
        Just enrichedArticle ->
            return $ BoxedArticle $ mapEnrichedArticleToArticleData enrichedArticle

createNewArticleHandler :: User -> BoxedArticle NewArticleData -> AppM (BoxedArticle ArticleData)
createNewArticleHandler user (BoxedArticle newArticle) = do
    currentTime <- liftIO getCurrentTime
    slug <- mkSlug title
    article <- ArticleDb.createArticle $ Article
                                            { articleAuthorId    = userId user
                                            , articleId          = ArticleId 0
                                            , articleTitle       = title
                                            , articleSlug        = slug
                                            , articleDescription = description
                                            , articleBody        = body
                                            , articleTags        = tags
                                            , articleCreatedAt   = currentTime
                                            , articleUpdatedAt   = currentTime
                                            }
    case article of
        Nothing       -> throwIO err400
        Just article' ->
            return $ BoxedArticle $ mapArticleToArticleData article' (mapUserToUserProfile user False) False 0
    where
        title = newArticleTitle newArticle
        description = newArticleDescription newArticle
        body = newArticleBody newArticle
        tags = newArticleTagList newArticle

updateArticleHandler :: User -> Slug -> BoxedArticle UpdateArticleData -> AppM (BoxedArticle ArticleData)
updateArticleHandler user slug (BoxedArticle updateData) =
    ArticleDb.getArticleBySlug slug >>= \case
        Nothing      -> throwIO err404
        Just article ->
            if articleAuthorId article /= userId user
            then
                throwIO err403
            else do
                currentTime <- liftIO getCurrentTime
                let updatedArticle = article
                                    { articleTitle       = fromMaybe (articleTitle article) (updateArticleTitle updateData)
                                    , articleDescription = fromMaybe (articleDescription article) (updateArticleDescription updateData)
                                    , articleBody        = fromMaybe (articleBody article) (updateArticleBody updateData)
                                    , articleTags        = fromMaybe (articleTags article) (updateArticleTagList updateData)
                                    , articleUpdatedAt   = currentTime
                                    }
                ArticleDb.updateArticle updatedArticle
                favoritedCount <- ArticleDb.getArticleFavoritedCount (articleId article)
                return $ BoxedArticle $ mapArticleToArticleData updatedArticle (mapUserToUserProfile user False) False favoritedCount

deleteArticleHandler :: User -> Slug -> AppM NoContent
deleteArticleHandler user slug = ArticleDb.getArticleBySlug slug >>= \case
    Nothing      -> throwIO err404
    Just article ->
        if articleAuthorId article /= userId user
        then
            throwIO err403
        else do
            ArticleDb.deleteArticleById (articleId article)
            return NoContent

favoriteArticleHandler :: User -> Slug -> AppM (BoxedArticle ArticleData)
favoriteArticleHandler user slug =
    ArticleDb.getArticleBySlug slug >>= \case
        Nothing      -> throwIO err404
        Just article ->
            if articleAuthorId article == userId user
            then
                throwIO err403
            else do
                isFavorited <- ArticleDb.checkFavorite user (articleId article)
                if isFavorited
                then
                    throwIO err400
                else do
                    success <- ArticleDb.addFavorite user (articleId article)
                    if success then
                        getArticleByIdHandler (Just user) (articleId article)
                    else throwIO err400

unFavoriteArticleHandler :: User -> Slug -> AppM (BoxedArticle ArticleData)
unFavoriteArticleHandler user slug =
    ArticleDb.getArticleBySlug slug >>= \case
        Nothing      -> throwIO err404
        Just article -> do
            isFavorited <- ArticleDb.checkFavorite user (articleId article)
            if not isFavorited
            then
                throwIO err400
            else do
                success <- ArticleDb.removeFavorite user (articleId article)
                if success then
                    getArticleByIdHandler (Just user) (articleId article)
                else throwIO err400

articleServer :: ServerT ArticleApi AppM
articleServer = getArticlesHandler
            :<|> getArticleBySlugHandler
            :<|> createNewArticleHandler
            :<|> updateArticleHandler
            :<|> deleteArticleHandler
            :<|> favoriteArticleHandler
            :<|> unFavoriteArticleHandler
