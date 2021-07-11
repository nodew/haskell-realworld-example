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
