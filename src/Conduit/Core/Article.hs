{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conduit.Core.Article where

import RIO
import Rel8
import Data.Aeson
import Data.Time

import Conduit.Core.User

data Article = Article
    { articleId          :: ArticleId
    , articleAuthorId    :: UserId
    , articleTitle       :: Text
    , articleSlug        :: Slug
    , articleDescription :: Text
    , articleBody        :: Text
    , articleCreatedAt   :: UTCTime
    , articleUpdatedAt   :: UTCTime
    }

newtype ArticleId = ArticleId { getArticleId :: Int64 }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newtype Slug = Slug { getSlug :: Text }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newtype TagId = TagId { getTagId :: Text }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

