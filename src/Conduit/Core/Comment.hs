{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conduit.Core.Comment where

import RIO
import Rel8
import Data.Aeson
import Data.Time

import Conduit.Core.User
import Conduit.Core.Article

data Comment = Comment
    { commentId          :: CommentId
    , commentBody        :: Text
    , commentArticleId   :: ArticleId
    , commentAuthorId    :: UserId 
    , commentCreatedAt   :: UTCTime
    , commentUpdatedAt   :: UTCTime
    }

newtype CommentId = CommentId { getCommentId :: Int64 }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)
