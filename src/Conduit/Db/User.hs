{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
module Conduit.Db.User where

import RIO
import Rel8

import Conduit.App
import Conduit.Core.User
import Conduit.Core.Password
import Conduit.Db.Schema.User
import Conduit.Db.Schema.UserFollow
import Conduit.Db.Helper

getUserById :: UserId -> AppM (Maybe User)
getUserById uid = withTransaction $ \transaction -> do
    users <- transaction $ do
        runStmt $ select $ getUserByIdStmt (litExpr uid)
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByName :: Username -> AppM (Maybe User)
getUserByName username = withTransaction $ \transaction -> do
    users <- transaction $ do
        runStmt $ select $ getUserByNameStmt username
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmail :: EmailAddress -> AppM (Maybe User)
getUserByEmail email = withTransaction $ \transaction -> do
    users <- transaction $ do
        runStmt $ select $ getUserByEmailStmt email
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmailAndPassword :: EmailAddress -> Password  -> AppM (Maybe User)
getUserByEmailAndPassword email password = withTransaction $ \transaction -> do
    users <- transaction $ do
        runStmt $ select $ getUserByEmailStmt email
    return $ verifyPassword' =<< listToMaybe users
    where
        verifyPassword' user =
            if verifyPassword password (entityUserSalt user) (entityUserPassword user)
            then
                Just $ mapUserEntityToUser user
            else
                Nothing

saveNewUser :: User -> Password -> AppM (Maybe User)
saveNewUser user password = withTransaction $ \transaction -> do
    hashedPwdAndSalt <- liftIO $ hashPassword password
    let stmt = insertUserStmt user hashedPwdAndSalt
    userIds <- transaction $ do
        runStmt $ insert stmt
    return $ listToMaybe userIds >>= \case
        uid -> Just $ user { userId = uid }

updateUser :: User -> Maybe Password -> AppM Bool
updateUser user mbPassword = withTransaction $ \transaction -> do
    hashAndSalt <-
        case mbPassword of
            Just password -> liftIO $ Just <$> hashPassword password
            _             -> return Nothing
    rows <- transaction $ do
        runStmt $ update $ updateUserStmt user hashAndSalt
    return $ rows > 0

checkFollowship :: User -> UserId -> AppM Bool
checkFollowship user following = withTransaction $ \transaction -> do
    exists <- transaction $ do
        runStmt $ select $ checkFollowshipStmt user (litExpr following)
    return $ exists == [True]

followUser :: User -> UserId -> AppM Bool
followUser user toFollow = withTransaction $ \transaction -> do
    rows <- transaction $ do
        runStmt $ insert $ createFollowshipStmt (userId user) toFollow
    return $ rows == 1

unfollowUser :: User -> UserId -> AppM Bool
unfollowUser user following = withTransaction $ \transaction -> do
    rows <- transaction $ do
        runStmt $ delete $ removeFollowshipStmt (userId user) following
    return $ rows == 1
