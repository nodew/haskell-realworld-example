{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Schema.User where

import RIO hiding (set)
import Rel8
import Hasql.Connection ( Connection )

import Conduit.Core.User

data UserEntity f = UserEntity
    { _userId       :: Column f UserId
    , _userName     :: Column f Username
    , _userEmail    :: Column f EmailAddress
    , _userPassword :: Column f Text
    , _userSalt     :: Column f Text
    , _userBio      :: Column f Text
    , _userImage    :: Column f Text
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (UserEntity f)

userSchema :: TableSchema (UserEntity Name)
userSchema = TableSchema
    { name = "users"
    , schema = Nothing
    , columns = UserEntity
        { _userId       = "user_id"
        , _userName     = "user_username"
        , _userEmail    = "user_email"
        , _userPassword = "user_password"
        , _userSalt     = "user_salt"
        , _userBio      = "user_bio"
        , _userImage    = "user_image"
        }
    }

mapUserEntityToUser :: UserEntity Result -> User
mapUserEntityToUser entity = User
    { userId     = _userId entity
    , userName   = _userName entity
    , userEmail  = _userEmail entity
    , userBio    = _userBio entity
    , userImage  = _userImage entity
    }

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

getUserByIdStmt :: Expr UserId -> Query (UserEntity Expr)
getUserByIdStmt uid = do
    a <- each userSchema
    where_ $ _userId a ==. uid
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
