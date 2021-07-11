{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Schema.UserFollow where

import RIO
import Rel8

import Conduit.App
import Conduit.Core.User

data UserFollowEntity f = UserFollowEntity
    { fwsUserId          :: Column f UserId
    , fwsFollowingUserId :: Column f UserId
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (UserFollowEntity f)

userFollowSchema :: TableSchema (UserFollowEntity Name)
userFollowSchema = TableSchema
    { name = "follows"
    , schema = Nothing
    , columns = UserFollowEntity
        { fwsUserId = "fws_user_id"
        , fwsFollowingUserId = "fws_follows_user_id"
        }
    }

