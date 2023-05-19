{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Conduit.Core.Comment where

import Conduit.Core.Article
import Conduit.Core.User
import Data.Aeson
import Data.Time
import Data.UUID
import RIO
import Rel8

data Comment = Comment
    { commentId :: CommentId
    , commentUUID :: UUID
    , commentBody :: Text
    , commentArticleId :: ArticleId
    , commentAuthorId :: UserId
    , commentCreatedAt :: UTCTime
    , commentUpdatedAt :: UTCTime
    }

newtype CommentId = CommentId {getCommentId :: Int64}
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)
