{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Conduit.Repository.User where

import Conduit.App
import Conduit.Core.Password
import Conduit.Core.User
import Conduit.Db
import RIO
import Rel8

getUserById :: UserId -> AppM (Maybe User)
getUserById uid = do
    users <- executeStmt $ select $ getUserByIdStmt (litExpr uid)
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByName :: Username -> AppM (Maybe User)
getUserByName username = do
    users <- executeStmt $ select $ getUserByNameStmt username
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmail :: EmailAddress -> AppM (Maybe User)
getUserByEmail email = do
    users <- executeStmt $ select $ getUserByEmailStmt email
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmailAndPassword :: EmailAddress -> Password -> AppM (Maybe User)
getUserByEmailAndPassword email password = do
    users <- executeStmt $ select $ getUserByEmailStmt email
    return $ verifyPassword' =<< listToMaybe users
    where
        verifyPassword' user =
            if verifyPassword password (entityUserPassword user)
                then Just $ mapUserEntityToUser user
                else Nothing

saveNewUser :: User -> Password -> AppM (Maybe User)
saveNewUser user password = do
    hashedPwdAndSalt <- liftIO $ hashPassword password
    userIds <- executeStmt $ insert $ insertUserStmt user hashedPwdAndSalt
    return $ listToMaybe userIds >>= \uid -> Just $ user {userId = uid}

updateUser :: User -> Maybe Password -> AppM Bool
updateUser user mbPassword = do
    hashAndSalt <-
        case mbPassword of
            Just password -> liftIO $ Just <$> hashPassword password
            _ -> return Nothing
    rows <- executeStmt $ update $ updateUserStmt user hashAndSalt
    return $ rows > 0

checkFollowship :: User -> UserId -> AppM Bool
checkFollowship user following = do
    exists <- executeStmt $ select $ checkFollowshipStmt user (litExpr following)
    return $ exists == [True]

followUser :: User -> UserId -> AppM Bool
followUser user toFollow = do
    rows <- executeStmt $ insert $ createFollowshipStmt (userId user) toFollow
    return $ rows == 1

unfollowUser :: User -> UserId -> AppM Bool
unfollowUser user following = do
    rows <- executeStmt $ delete $ removeFollowshipStmt (userId user) following
    return $ rows == 1
