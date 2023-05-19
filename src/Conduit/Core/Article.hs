{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Conduit.Core.Article where

import Conduit.Core.User
import Crypto.Hash (SHA256 (SHA256), hashWith)
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson
import qualified Data.Text as T
import Data.Time
import RIO
import Rel8
import Servant

data Article = Article
    { articleId :: ArticleId
    , articleAuthorId :: UserId
    , articleTitle :: Text
    , articleSlug :: Slug
    , articleDescription :: Text
    , articleBody :: Text
    , articleTags :: [Text]
    , articleCreatedAt :: UTCTime
    , articleUpdatedAt :: UTCTime
    }

newtype ArticleId = ArticleId {getArticleId :: Int64}
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newtype Slug = Slug {getSlug :: Text}
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, FromHttpApiData, DBEq, DBType)

newtype TagId = TagId {getTagId :: Int64}
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

mkSlug :: MonadIO m => Text -> m Slug
mkSlug title = do
    rnd <- liftIO $ getRandomBytes 32
    let hash = T.pack $ show $ hashWith SHA256 (rnd :: ByteString)
    let slugText = T.intercalate "-" $ (T.words . T.toLower $ title) ++ [T.take 8 hash]
    return $ Slug slugText
