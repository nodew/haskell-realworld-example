{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Conduit.Db.Schema.Comment where

import Conduit.Core.Article
import Conduit.Core.Comment
import Conduit.Core.User
import Data.Time
import Data.UUID
import RIO
import Rel8
import Rel8.Expr.Time

data CommentEntity f = CommentEntity
    { entityCommentId :: Column f CommentId
    , entityCommentUUID :: Column f UUID
    , entityCommentBody :: Column f Text
    , entityCommentArticleId :: Column f ArticleId
    , entityCommentAuthorId :: Column f UserId
    , entityCommentCreatedAt :: Column f UTCTime
    , entityCommentUpdatedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (CommentEntity f)

commentSchema :: TableSchema (CommentEntity Name)
commentSchema =
    TableSchema
        { name = "comments"
        , schema = Nothing
        , columns =
            CommentEntity
                { entityCommentId = "comment_id"
                , entityCommentUUID = "comment_uuid"
                , entityCommentBody = "comment_body"
                , entityCommentArticleId = "comment_article_id"
                , entityCommentAuthorId = "comment_user_id"
                , entityCommentCreatedAt = "comment_createdat"
                , entityCommentUpdatedAt = "comment_updatedat"
                }
        }

mapCommentEntityToComment :: CommentEntity Result -> Comment
mapCommentEntityToComment entity =
    Comment
        { commentId = entityCommentId entity
        , commentUUID = entityCommentUUID entity
        , commentBody = entityCommentBody entity
        , commentArticleId = entityCommentArticleId entity
        , commentAuthorId = entityCommentAuthorId entity
        , commentCreatedAt = entityCommentCreatedAt entity
        , commentUpdatedAt = entityCommentUpdatedAt entity
        }

getCommentByIdStmt :: Expr CommentId -> Query (CommentEntity Expr)
getCommentByIdStmt commentId = do
    comment <- each commentSchema
    where_ $ entityCommentId comment ==. commentId
    return comment

getCommentByUUIDStmt :: Expr UUID -> Query (CommentEntity Expr)
getCommentByUUIDStmt uuid = do
    comment <- each commentSchema
    where_ $ entityCommentUUID comment ==. uuid
    return comment

getCommentsByArticleIdStmt :: Expr ArticleId -> Query (CommentEntity Expr)
getCommentsByArticleIdStmt articleId = do
    comment <- each commentSchema
    where_ $ entityCommentArticleId comment ==. articleId
    return comment

insertCommentStmt :: Comment -> Insert [CommentId]
insertCommentStmt comment =
    Insert
        { into = commentSchema
        , rows =
            values
                [ CommentEntity
                    { entityCommentId = unsafeCastExpr $ nextval "comments_comment_id_seq"
                    , entityCommentUUID = lit $ commentUUID comment
                    , entityCommentBody = lit $ commentBody comment
                    , entityCommentArticleId = lit $ commentArticleId comment
                    , entityCommentAuthorId = lit $ commentAuthorId comment
                    , entityCommentCreatedAt = now
                    , entityCommentUpdatedAt = now
                    }
                ]
        , onConflict = Abort
        , returning = Projection entityCommentId
        }

deleteCommentStmt :: CommentId -> Delete Int64
deleteCommentStmt commentId =
    Delete
        { from = commentSchema
        , using = pure ()
        , deleteWhere = \_ row -> entityCommentId row ==. lit commentId
        , returning = NumberOfRowsAffected
        }

deleteCommentByArticleIdStmt :: ArticleId -> Delete Int64
deleteCommentByArticleIdStmt articleId =
    Delete
        { from = commentSchema
        , using = pure ()
        , deleteWhere = \_ row -> entityCommentArticleId row ==. lit articleId
        , returning = NumberOfRowsAffected
        }
