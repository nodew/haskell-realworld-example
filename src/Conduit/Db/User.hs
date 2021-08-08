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
import Conduit.Util

getUserById :: UserId -> AppM (Maybe User)
getUserById uid = do
    users <- runSimpleStmt $ select $ getUserByIdStmt (litExpr uid)
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByName :: Username -> AppM (Maybe User)
getUserByName username = do
    users <- runSimpleStmt $ select $ getUserByNameStmt username
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmail :: EmailAddress -> AppM (Maybe User)
getUserByEmail email = do
    users <- runSimpleStmt $ select $ getUserByEmailStmt email
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmailAndPassword :: EmailAddress -> Password  -> AppM (Maybe User)
getUserByEmailAndPassword email password = do
    users <- runSimpleStmt $ select $ getUserByEmailStmt email
    return $ verifyPassword' =<< listToMaybe users
    where
        verifyPassword' user =
            if verifyPassword password (entityUserSalt user) (entityUserPassword user)
            then
                Just $ mapUserEntityToUser user
            else
                Nothing

saveNewUser :: User -> Password -> AppM (Maybe User)
saveNewUser user password = do
    hashedPwdAndSalt <- liftIO $ hashPassword password
    userIds <- runSimpleStmt $ insert $ insertUserStmt user hashedPwdAndSalt
    return $ listToMaybe userIds >>= \uid -> Just $ user { userId = uid }

updateUser :: User -> Maybe Password -> AppM Bool
updateUser user mbPassword = do
    hashAndSalt <-
        case mbPassword of
            Just password -> liftIO $ Just <$> hashPassword password
            _             -> return Nothing
    rows <- runSimpleStmt $ update $ updateUserStmt user hashAndSalt
    return $ rows > 0

checkFollowship :: User -> UserId -> AppM Bool
checkFollowship user following = do
    exists <- runSimpleStmt $ select $ checkFollowshipStmt user (litExpr following)
    return $ exists == [True]

followUser :: User -> UserId -> AppM Bool
followUser user toFollow = do
    rows <-runSimpleStmt $ insert $ createFollowshipStmt (userId user) toFollow
    return $ rows == 1

unfollowUser :: User -> UserId -> AppM Bool
unfollowUser user following = do
    rows <- runSimpleStmt $ delete $ removeFollowshipStmt (userId user) following
    return $ rows == 1
