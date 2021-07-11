{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Conduit.Db.Schema.Comment where

import RIO
import Rel8
import Data.Time

import Conduit.Core.User
import Conduit.Core.Article
import Conduit.Core.Comment

data CommentEntity f = CommentEntity
    { _commentId        :: Column f CommentId
    , _commentBody      :: Column f Text
    , _commentArticleId :: Column f ArticleId
    , _commentAuthorId  :: Column f UserId 
    , _commentCreatedAt :: Column f UTCTime
    , _commentUpdatedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (CommentEntity f)

commentSchema :: TableSchema (CommentEntity Name)
commentSchema = TableSchema
    { name = "comments"
    , schema = Nothing
    , columns = CommentEntity
        { _commentId        = "comment_id"
        , _commentBody      = "comment_body"
        , _commentArticleId = "comment_article_id"
        , _commentAuthorId  = "comment_user_id"
        , _commentCreatedAt = "comment_createdAt"
        , _commentUpdatedAt = "comment_updatedAt"
        }
    }

mapCommentEntityToComment :: CommentEntity Result -> Comment
mapCommentEntityToComment entity = Comment
    { commentId        = _commentId entity
    , commentBody      = _commentBody entity
    , commentArticleId = _commentArticleId entity
    , commentAuthorId  = _commentAuthorId entity
    , commentCreatedAt = _commentCreatedAt entity
    , commentUpdatedAt = _commentUpdatedAt entity
    }