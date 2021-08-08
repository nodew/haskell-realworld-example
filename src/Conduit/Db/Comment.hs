{-# LANGUAGE RankNTypes #-}
module Conduit.Db.Comment where

import RIO
import Rel8
import Data.UUID

import Conduit.App
import Conduit.Core.User
import Conduit.Core.Article
import Conduit.Core.Comment
import Conduit.Db.Schema.Comment
import Conduit.Db.Schema.User
import Conduit.Db.Schema.UserFollow
import Conduit.Db.Helper

type EnrichedComment = (Comment, User, Bool )

getEnrichedCommentsByArticleId :: Maybe User -> ArticleId -> AppM [EnrichedComment]
getEnrichedCommentsByArticleId mbUser articleId = do
    results <- runSimpleStmt $ select $ do
        comment <- getCommentsByArticleIdStmt (litExpr articleId)
        author <- getUserByIdStmt (entityCommentAuthorId comment)
        followingAuthor <- maybe (return $ litExpr False) (\user -> checkFollowshipStmt user (entityCommentAuthorId comment)) mbUser
        return (comment, author, followingAuthor)
    return $ map (\(comment, author, following) -> (mapCommentEntityToComment comment, mapUserEntityToUser author, following)) results

getCommentById :: CommentId -> AppM (Maybe Comment)
getCommentById commentId = do
    comments <- runSimpleStmt $ select $ getCommentByIdStmt (litExpr commentId)
    return $ listToMaybe $ map mapCommentEntityToComment comments

getCommentByUUID :: UUID -> AppM (Maybe Comment)
getCommentByUUID  uuid = do
    comments <- runSimpleStmt $ select $ getCommentByUUIDStmt (litExpr uuid)
    return $ listToMaybe $ map mapCommentEntityToComment comments

addComment :: Comment -> AppM (Maybe Comment)
addComment comment = do
    commentId <- runSimpleStmt $ insert (insertCommentStmt comment)
    return $ updateCommentId =<< listToMaybe commentId
    where
        updateCommentId commentId' = Just $ comment {commentId = commentId' }

deleteCommentById :: CommentId -> AppM Bool
deleteCommentById commentId = do
    changedRows <- runSimpleStmt $ delete $ deleteCommentStmt commentId
    return $ changedRows == 1
