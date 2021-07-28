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

type ArticleApi = AuthProtect "Optional"
                        :> "articles"
                        :> QueryParam "page"      Int64
                        :> QueryParam "pageSize"  Int64
                        :> QueryParam "author"    Text
                        :> QueryParam "favorited" Text
                        :> QueryParam "tag"       Text
                        :> Get '[JSON] ArticlesResponse
                :<|> AuthProtect "Required"
                        :> "articles"
                        :> ReqBody '[JSON] (BoxedArticle NewArticleData)
                        :> Post '[JSON] (BoxedArticle ArticleData)
                :<|> AuthProtect "Required"
                        :> "articles"
                        :> Capture "slug" Text
                        :> ReqBody '[JSON] (BoxedArticle UpdateArticleData)
                        :> Put '[JSON] (BoxedArticle ArticleData)
                :<|> AuthProtect "Optional"
                        :> "articles"
                        :> Capture "slug" Text
                        :> Get '[JSON] (BoxedArticle ArticleData)

getArticlesHandler :: Maybe User
                   -> Maybe Int64
                   -> Maybe Int64
                   -> Maybe Text
                   -> Maybe Text
                   -> Maybe Text
                   -> AppM ArticlesResponse
getArticlesHandler maybeUser page pageSize authorName favorited tag = do
    (pagedResults, t) <- do
        tagId <- maybe (return Nothing) ArticleDb.getTagId tag
        withValidFilter tag tagId $ do
            author <- maybe (return Nothing) UserDb.getUserByName (Username <$> authorName)
            withValidFilter authorName author $ do
                favoritedBy <- maybe (return Nothing) UserDb.getUserByName (Username <$> favorited)
                withValidFilter favorited favoritedBy $ do
                    let filters = ArticleDb.ArticleFilters tagId (userId <$> favoritedBy) (userId <$> author)
                    let pageParam = ArticleDb.Pagination (fromMaybe 0 page) (fromMaybe 20 pageSize)
                    ArticleDb.getAllArticle maybeUser pageParam filters

    return $ ArticlesResponse t
        $ map (\(article, author, followingAuthor, favorited, favoritedCount) ->
                    mapArticleToArticleData article (mapUserToUserProfile author followingAuthor) favorited favoritedCount)
            pagedResults
    where
        isValidParam :: Maybe a -> Maybe b -> Bool
        isValidParam (Just _) Nothing = False
        isValidParam _        _       = True

        withValidFilter :: forall a b c . Maybe a -> Maybe b -> AppM ([c], Int64) -> AppM ([c], Int64)
        withValidFilter a b m
            | isValidParam a b = m
            | otherwise        = return ([], 0)

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


updateArticleHandler :: User -> Text -> BoxedArticle UpdateArticleData -> AppM (BoxedArticle ArticleData)
updateArticleHandler =
    undefined

getArticleBySlugHandler :: Maybe User -> Text -> AppM (BoxedArticle ArticleData)
getArticleBySlugHandler maybeUser slug = do
    result <- ArticleDb.getCompletedArticleBySlug maybeUser (Slug slug)
    case result of
        Nothing -> throwIO err404
        Just (article, author, followingAuthor, favorited, favoritedCount) ->
            return $ BoxedArticle $ mapArticleToArticleData article (mapUserToUserProfile author followingAuthor) favorited favoritedCount

articleServer :: ServerT ArticleApi AppM
articleServer = getArticlesHandler
            :<|> createNewArticleHandler
            :<|> updateArticleHandler
            :<|> getArticleBySlugHandler
