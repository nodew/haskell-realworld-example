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

updateUserExprWithUser :: User -> UserEntity Expr -> UserEntity Expr
updateUserExprWithUser user expr = expr
                                { _userName = lit (userName user)
                                , _userEmail = lit (userEmail user)
                                , _userBio = lit (userBio user)
                                , _userImage = lit (userImage user)
                                }

updateUserExprWithPassword :: (Text, Text) -> UserEntity Expr ->  UserEntity Expr
updateUserExprWithPassword (hash, salt) expr = expr
                                {  _userSalt = lit salt
                                , _userPassword = lit hash
                                }

getUserByIdStmt :: UserId -> Query (UserEntity Expr)
getUserByIdStmt uid = do
    a <- each userSchema
    where_ $ _userId a ==. lit uid
    return a

getUserByNameStmt :: Username -> Query (UserEntity Expr)
getUserByNameStmt name = do
    a <- each userSchema
    where_ $ _userName a ==. lit name
    return a

getUserByEmailStmt :: EmailAddress  -> Query (UserEntity Expr)
getUserByEmailStmt email = do
    a <- each userSchema
    where_ $ _userEmail a ==. lit email
    return a

insertUserStmt :: Username -> EmailAddress -> Text -> Text -> Insert [UserId]
insertUserStmt username email hash salt = Insert
    { into = userSchema
    , rows = [ UserEntity
                { _userId = unsafeCastExpr $ nextval "users_user_id_seq"
                , _userName = lit username
                , _userEmail = lit email
                , _userPassword = lit hash
                , _userSalt = lit salt
                , _userBio  = ""
                , _userImage = ""
                }
            ]
    , onConflict = DoNothing
    , returning = Projection _userId
    }

updateUserStmt :: User -> Maybe (Text, Text) -> Update Int64
updateUserStmt user maybePassword =
    Update
        { target = userSchema
        , updateWhere = \o -> _userId o ==. lit (userId user)
        , set = setter
        , returning = NumberOfRowsAffected
        }
    where
        setter = case maybePassword of
            Just pass -> updateUserExprWithPassword pass . updateUserExprWithUser user
            _         -> updateUserExprWithUser user

{-------------------------------------------------------------------------------}

getUserById' :: forall m . MonadIO m => Connection -> UserId -> m (Maybe User)
getUserById' conn uid = do
    users <- liftIO $ select conn $ getUserByIdStmt uid
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
            if verifyPassword password (_userSalt user) (_userPassword user) then
                Just $ mapUserEntityToUser user
            else
                Nothing

saveNewUser' :: forall m . MonadIO m => Connection -> User -> Password -> m (Maybe User)
saveNewUser' conn user password = do
    (hash, salt) <- liftIO $ hashPassword password
    let stmt = insertUserStmt (userName user) (userEmail user) hash salt
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
    rows <- liftIO $ select conn $ do
        a <- each userFollowSchema
        where_ $ (fwsUserId a ==. lit (userId user)) &&. (fwsFollowingUserId a ==. lit following)
        return a

    return $ not . RIO.null $ rows

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
    return $ rows > 0

unfollowUser' :: forall m . MonadIO m => Connection -> User -> UserId -> m Bool
unfollowUser' conn user following = do
    rows <- liftIO $ delete conn $ Delete
        { from = userFollowSchema
        , deleteWhere = \o -> (fwsUserId o ==. lit (userId user)) &&. (fwsFollowingUserId o ==. lit following)
        , returning = NumberOfRowsAffected
        }
    return $ rows > 0

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
