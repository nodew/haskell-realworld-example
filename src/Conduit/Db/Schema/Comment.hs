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
    { entityCommentId        :: Column f CommentId
    , entityCommentBody      :: Column f Text
    , entityCommentArticleId :: Column f ArticleId
    , entityCommentAuthorId  :: Column f UserId
    , entityCommentCreatedAt :: Column f UTCTime
    , entityCommentUpdatedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (CommentEntity f)

commentSchema :: TableSchema (CommentEntity Name)
commentSchema = TableSchema
    { name = "comments"
    , schema = Nothing
    , columns = CommentEntity
        { entityCommentId        = "comment_id"
        , entityCommentBody      = "comment_body"
        , entityCommentArticleId = "comment_article_id"
        , entityCommentAuthorId  = "comment_user_id"
        , entityCommentCreatedAt = "comment_createdat"
        , entityCommentUpdatedAt = "comment_updatedat"
        }
    }

mapCommentEntityToComment :: CommentEntity Result -> Comment
mapCommentEntityToComment entity = Comment
    { commentId        = entityCommentId entity
    , commentBody      = entityCommentBody entity
    , commentArticleId = entityCommentArticleId entity
    , commentAuthorId  = entityCommentAuthorId entity
    , commentCreatedAt = entityCommentCreatedAt entity
    , commentUpdatedAt = entityCommentUpdatedAt entity
    }
