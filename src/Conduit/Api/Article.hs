module Conduit.Api.Article where

import Conduit.Api.Common
import Conduit.App
import Conduit.Core.Article
import Conduit.Core.User
import qualified Conduit.Repository.Article as ArticleRepository
import qualified Conduit.Repository.User as UserRepository
import Conduit.Util
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Time
import RIO
import Servant

data NewArticleData = NewArticleData
    { newArticleTitle :: Text
    , newArticleDescription :: Text
    , newArticleBody :: Text
    , newArticleTagList :: [Text]
    }
    deriving (Eq, Show, Generic)

instance FromJSON NewArticleData where
    parseJSON = genericParseJSON $ toJsonOptions 10

instance ToJSON NewArticleData where
    toJSON = genericToJSON $ toJsonOptions 10

data UpdateArticleData = UpdateArticleData
    { updateArticleTitle :: Maybe Text
    , updateArticleDescription :: Maybe Text
    , updateArticleBody :: Maybe Text
    , updateArticleTagList :: Maybe [Text]
    }
    deriving (Show, Generic)

instance FromJSON UpdateArticleData where
    parseJSON = genericParseJSON $ toJsonOptions 13

data ArticleData = ArticleData
    { articleDataSlug :: Text
    , articleDataTitle :: Text
    , articleDataDescription :: Text
    , articleDataBody :: Text
    , articleDataTagList :: [Text]
    , articleDataCreatedAt :: UTCTime
    , articleDataUpdatedAt :: UTCTime
    , articleDataFavorited :: Bool
    , articleDataFavoritesCount :: Int64
    , articleDataAuthor :: UserProfile
    }
    deriving (Eq, Show, Generic)

instance ToJSON ArticleData where
    toJSON = genericToJSON $ toJsonOptions 11

instance FromJSON ArticleData where
    parseJSON = genericParseJSON $ toJsonOptions 11

newtype BoxedArticle a = BoxedArticle
    {boxedArticle :: a}
    deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (BoxedArticle a) where
    toJSON = genericToJSON $ toJsonOptions 5

instance FromJSON a => FromJSON (BoxedArticle a) where
    parseJSON = genericParseJSON $ toJsonOptions 5

data ArticlesResponse = ArticlesResponse
    { articlesRespArticlesCount :: Int64
    , articlesRespArticles :: [ArticleData]
    }
    deriving (Eq, Show, Generic)

instance ToJSON ArticlesResponse where
    toJSON = genericToJSON $ toJsonOptions 12

instance FromJSON ArticlesResponse where
    parseJSON = genericParseJSON $ toJsonOptions 12

mapArticleToArticleData :: Article -> UserProfile -> Bool -> Int64 -> ArticleData
mapArticleToArticleData article authorProfile favorited favoritedCount =
    ArticleData
        { articleDataSlug = getSlug $ articleSlug article
        , articleDataTitle = articleTitle article
        , articleDataDescription = articleDescription article
        , articleDataBody = articleBody article
        , articleDataTagList = articleTags article
        , articleDataCreatedAt = articleCreatedAt article
        , articleDataUpdatedAt = articleUpdatedAt article
        , articleDataFavorited = favorited
        , articleDataFavoritesCount = favoritedCount
        , articleDataAuthor = authorProfile
        }

mapEnrichedArticleToArticleData :: ArticleRepository.EnrichedArticle -> ArticleData
mapEnrichedArticleToArticleData (ArticleRepository.EnrichedArticle article author followingAuthor favorited favoritedCount) =
    mapArticleToArticleData article (mapUserToUserProfile author followingAuthor) favorited favoritedCount

type ArticleApi =
    AuthProtect "Optional"
        :> "articles"
        :> QueryParam "page" Int64
        :> QueryParam "pageSize" Int64
        :> QueryParam "author" Text
        :> QueryParam "favorited" Text
        :> QueryParam "tag" Text
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

getArticlesHandler ::
    Maybe User ->
    Maybe Int64 ->
    Maybe Int64 ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    AppM ArticlesResponse
getArticlesHandler mbUser mbPage mbPageSize mbAuthorName mbFavorited mbTag = do
    (pagedResults, total) <- do
        tagId <- maybe (return Nothing) ArticleRepository.getTagId mbTag
        withValidFilter mbTag tagId $ do
            author <- maybe (return Nothing) UserRepository.getUserByName (Username <$> mbAuthorName)
            withValidFilter mbAuthorName author $ do
                favoritedBy <- maybe (return Nothing) UserRepository.getUserByName (Username <$> mbFavorited)
                withValidFilter mbFavorited favoritedBy $ do
                    let filters = ArticleRepository.ArticleFilters tagId (userId <$> favoritedBy) (userId <$> author)
                    let pageParam = ArticleRepository.Pagination (fromMaybe 0 mbPage) (fromMaybe 20 mbPageSize)
                    ArticleRepository.getPagedArticle mbUser pageParam filters

    return $ ArticlesResponse total $ map mapEnrichedArticleToArticleData pagedResults
    where
        isValidParam :: Maybe a -> Maybe b -> Bool
        isValidParam (Just _) Nothing = False
        isValidParam _ _ = True

        withValidFilter :: forall a b c. Maybe a -> Maybe b -> AppM ([c], Int64) -> AppM ([c], Int64)
        withValidFilter a b m
            | isValidParam a b = m
            | otherwise = return ([], 0)

getArticleBySlugHandler :: Maybe User -> Slug -> AppM (BoxedArticle ArticleData)
getArticleBySlugHandler mbUser slug = do
    ArticleRepository.getEnrichedArticleBySlug mbUser slug
        >>= maybe
            (throwIO err404)
            (return . BoxedArticle . mapEnrichedArticleToArticleData)

getArticleByIdHandler :: Maybe User -> ArticleId -> AppM (BoxedArticle ArticleData)
getArticleByIdHandler mbUser articleId =
    ArticleRepository.getEnrichedArticleById mbUser articleId
        >>= maybe
            (throwIO err404)
            (return . BoxedArticle . mapEnrichedArticleToArticleData)

createNewArticleHandler :: User -> BoxedArticle NewArticleData -> AppM (BoxedArticle ArticleData)
createNewArticleHandler user (BoxedArticle newArticle) = do
    currentTime <- liftIO getCurrentTime
    slug <- mkSlug title
    article <-
        ArticleRepository.createArticle $
            Article
                { articleAuthorId = userId user
                , articleId = ArticleId 0
                , articleTitle = title
                , articleSlug = slug
                , articleDescription = description
                , articleBody = body
                , articleTags = tags
                , articleCreatedAt = currentTime
                , articleUpdatedAt = currentTime
                }
    flipMaybe article (throwIO err400) $ \article' ->
        return $ BoxedArticle $ mapArticleToArticleData article' (mapUserToUserProfile user False) False 0
    where
        title = newArticleTitle newArticle
        description = newArticleDescription newArticle
        body = newArticleBody newArticle
        tags = newArticleTagList newArticle

updateArticleHandler :: User -> Slug -> BoxedArticle UpdateArticleData -> AppM (BoxedArticle ArticleData)
updateArticleHandler user slug (BoxedArticle updateData) =
    ArticleRepository.getArticleBySlug slug >>= maybe (throwIO err404) updateArticle
    where
        updateArticle article =
            if articleAuthorId article /= userId user
                then throwIO err403
                else do
                    currentTime <- liftIO getCurrentTime
                    let updatedArticle =
                            article
                                { articleTitle = fromMaybe (articleTitle article) (updateArticleTitle updateData)
                                , articleDescription = fromMaybe (articleDescription article) (updateArticleDescription updateData)
                                , articleBody = fromMaybe (articleBody article) (updateArticleBody updateData)
                                , articleTags = fromMaybe (articleTags article) (updateArticleTagList updateData)
                                , articleUpdatedAt = currentTime
                                }
                    ArticleRepository.updateArticle updatedArticle
                    favoritedCount <- ArticleRepository.getArticleFavoritedCount (articleId article)
                    return $ BoxedArticle $ mapArticleToArticleData updatedArticle (mapUserToUserProfile user False) False favoritedCount

deleteArticleHandler :: User -> Slug -> AppM NoContent
deleteArticleHandler user slug =
    ArticleRepository.getArticleBySlug slug >>= maybe (throwIO err404) deleteArticle
    where
        deleteArticle article =
            if articleAuthorId article /= userId user
                then throwIO err403
                else do
                    ArticleRepository.deleteArticleById (articleId article)
                    return NoContent

favoriteArticleHandler :: User -> Slug -> AppM (BoxedArticle ArticleData)
favoriteArticleHandler user slug =
    ArticleRepository.getArticleBySlug slug >>= maybe (throwIO err404) favoriteArticle
    where
        favoriteArticle article =
            if articleAuthorId article == userId user
                then throwIO err403
                else do
                    isFavorited <- ArticleRepository.checkFavorite user (articleId article)
                    if isFavorited
                        then throwIO err400
                        else do
                            success <- ArticleRepository.addFavorite user (articleId article)
                            if success
                                then getArticleByIdHandler (Just user) (articleId article)
                                else throwIO err400

unFavoriteArticleHandler :: User -> Slug -> AppM (BoxedArticle ArticleData)
unFavoriteArticleHandler user slug =
    ArticleRepository.getArticleBySlug slug >>= maybe (throwIO err404) unFavoriteArticle
    where
        unFavoriteArticle article = do
            isFavorited <- ArticleRepository.checkFavorite user (articleId article)
            if not isFavorited
                then throwIO err400
                else do
                    success <- ArticleRepository.removeFavorite user (articleId article)
                    if success
                        then getArticleByIdHandler (Just user) (articleId article)
                        else throwIO err400

articleServer :: ServerT ArticleApi AppM
articleServer =
    getArticlesHandler
        :<|> getArticleBySlugHandler
        :<|> createNewArticleHandler
        :<|> updateArticleHandler
        :<|> deleteArticleHandler
        :<|> favoriteArticleHandler
        :<|> unFavoriteArticleHandler
