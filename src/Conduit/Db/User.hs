{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
module Conduit.Db.User where

import RIO
import Rel8
import Hasql.Connection
import Control.Applicative

import Conduit.App
import Conduit.Core.User
import Conduit.Core.Password
import Conduit.Db.Schema.User
import Conduit.Db.Schema.UserFollow
import Conduit.Util

getUserById' :: forall m . MonadIO m => Connection -> UserId -> m (Maybe User)
getUserById' conn uid = do
    users <- liftIO $ select conn $ getUserByIdStmt (litExpr uid)
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByName' :: forall m . MonadIO m => Connection -> Username -> m (Maybe User)
getUserByName' conn name = do
    users <- liftIO $ select conn $ getUserByNameStmt name
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmail' :: forall m . MonadIO m => Connection -> EmailAddress -> m (Maybe User)
getUserByEmail' conn email = do
    users <- liftIO $ select conn $ getUserByEmailStmt email
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmailAndPassword' :: forall m . MonadIO m => Connection -> EmailAddress -> Password  -> m (Maybe User)
getUserByEmailAndPassword' conn email password = do
    users <- liftIO $ select conn $ getUserByEmailStmt email
    return $ listToMaybe users >>= \case
        user ->
            if verifyPassword password (entityUserSalt user) (entityUserPassword user) then
                Just $ mapUserEntityToUser user
            else
                Nothing

saveNewUser' :: forall m . MonadIO m => Connection -> User -> Password -> m (Maybe User)
saveNewUser' conn user password = do
    hashedPwdAndSalt <- liftIO $ hashPassword password
    let stmt = insertUserStmt user hashedPwdAndSalt
    userIds <- liftIO $ insert conn stmt
    return $ listToMaybe userIds >>= \case
        uid -> Just $ user { userId = uid }

updateUser' :: forall m . MonadIO m => Connection -> User -> Maybe Password -> m Bool
updateUser' conn user maybePassword = do
    hashAndSalt <-
        case maybePassword of
            Just password -> liftIO $ Just <$> hashPassword password
            _             -> return Nothing
    rows <- liftIO $ update conn $ updateUserStmt user hashAndSalt
    return $ rows > 0

checkFollowship' :: forall m . MonadIO m => Connection -> User -> UserId -> m Bool
checkFollowship' conn user following = do
    exists <- liftIO $ select conn $ checkFollowshipStmt user (litExpr following)
    return $ exists == [True]

followUser' :: forall m . MonadIO m => Connection -> User -> UserId -> m Bool
followUser' conn user toFollow = do
    rows <- liftIO $ insert conn $ Insert
        { into = userFollowSchema
        , rows = [ UserFollowEntity
                    { fwsUserId          = lit (userId user)
                    , fwsFollowingUserId = lit toFollow
                    }
                ]
        , onConflict = DoNothing
        , returning = NumberOfRowsAffected
        }
    return $ rows == 1

unfollowUser' :: forall m . MonadIO m => Connection -> User -> UserId -> m Bool
unfollowUser' conn user following = do
    rows <- liftIO $ delete conn $ Delete
        { from = userFollowSchema
        , deleteWhere = \o -> (fwsUserId o ==. lit (userId user)) &&. (fwsFollowingUserId o ==. lit following)
        , returning = NumberOfRowsAffected
        }
    return $ rows == 1

{--------------------------------------------------------------------------------}

getUserById :: UserId -> AppM (Maybe User)
getUserById = flipM getConn getUserById'

getUserByName :: Username -> AppM (Maybe User)
getUserByName = flipM getConn getUserByName'

getUserByEmail :: EmailAddress -> AppM (Maybe User)
getUserByEmail = flipM getConn getUserByEmail'

getUserByEmailAndPassword :: EmailAddress -> Password  -> AppM (Maybe User)
getUserByEmailAndPassword = flipM2 getConn getUserByEmailAndPassword'

saveNewUser :: User -> Password -> AppM (Maybe User)
saveNewUser = flipM2 getConn saveNewUser'

updateUser :: User -> Maybe Password -> AppM Bool
updateUser = flipM2 getConn updateUser'

checkFollowship :: User -> UserId -> AppM Bool
checkFollowship = flipM2 getConn checkFollowship'

followUser :: User -> UserId -> AppM Bool
followUser = flipM2 getConn followUser'

unfollowUser :: User -> UserId -> AppM Bool
unfollowUser = flipM2 getConn unfollowUser'
