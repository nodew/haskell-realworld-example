module Conduit.Db.Comment where

import RIO
import Rel8
import Data.UUID
import Control.Monad.Trans.Maybe
import Hasql.Connection

import Conduit.App
import Conduit.Core.User
import Conduit.Core.Article
import Conduit.Core.Comment
import Conduit.Db.Schema.Comment
import Conduit.Db.Schema.User
import Conduit.Db.Schema.UserFollow
import Conduit.Util

type EnrichedComment = (Comment, User, Bool )

getEnrichedCommentsByArticleId' :: MonadIO m
                        => Connection
                        -> Maybe User
                        -> ArticleId
                        -> m [EnrichedComment]
getEnrichedCommentsByArticleId' conn mbUser articleId = do
    results <- liftIO $ select conn $ do
        comment <- getCommentsByArticleIdStmt (litExpr articleId)
        author <- getUserByIdStmt (entityCommentAuthorId comment)
        followingAuthor <- maybe (return $ litExpr False) (\user -> checkFollowshipStmt user (entityCommentAuthorId comment)) mbUser
        return (comment, author, followingAuthor)
    return $ map (\(comment, author, following) -> (mapCommentEntityToComment comment, mapUserEntityToUser author, following)) results

getCommentById' :: MonadIO m => Connection -> Expr CommentId -> m (Maybe Comment)
getCommentById' conn commentId = do
    comments <- liftIO $ select conn $ getCommentByIdStmt commentId
    return $ listToMaybe $ map mapCommentEntityToComment comments

getCommentByUUID' :: MonadIO m => Connection -> UUID -> m (Maybe Comment)
getCommentByUUID' conn uuid = do
    comments <- liftIO $ select conn $ getCommentByUUIDStmt (litExpr uuid)
    return $ listToMaybe $ map mapCommentEntityToComment comments

addComment' :: MonadIO m => Connection -> Comment -> m (Maybe Comment)
addComment' conn comment = runMaybeT $ do
    commentId' <- MaybeT $ liftIO $ listToMaybe <$> insert conn (insertCommentStmt comment)
    return $ comment {commentId = commentId' }

deleteCommentById' :: MonadIO m => Connection -> CommentId -> m Bool
deleteCommentById' conn commentId = do
    changedRows <- liftIO $ delete conn $ deleteCommentStmt commentId
    return $ changedRows == 1

{--------------------------------------------------------------------------------------------------------------------}

getEnrichedCommentsByArticleId :: Maybe User -> ArticleId -> AppM [EnrichedComment]
getEnrichedCommentsByArticleId = flipM2 getConn getEnrichedCommentsByArticleId'

getCommentById :: Expr CommentId -> AppM (Maybe Comment)
getCommentById = flipM getConn getCommentById'

getCommentByUUID :: UUID -> AppM (Maybe Comment)
getCommentByUUID = flipM getConn getCommentByUUID'

addComment :: Comment -> AppM (Maybe Comment)
addComment = flipM getConn addComment'

deleteCommentById :: CommentId -> AppM Bool
deleteCommentById = flipM getConn deleteCommentById'
