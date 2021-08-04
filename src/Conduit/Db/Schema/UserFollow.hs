{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Schema.UserFollow where

import RIO
import Rel8

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
        { fwsUserId          = "fws_user_id"
        , fwsFollowingUserId = "fws_follows_user_id"
        }
    }

createFollowshipStmt :: UserId -> UserId -> Insert Int64
createFollowshipStmt currentUserId toFollowUserId =
    Insert
        { into = userFollowSchema
        , rows = values
            [ UserFollowEntity
                { fwsUserId = lit currentUserId
                , fwsFollowingUserId = lit toFollowUserId
                }
            ]
        , onConflict = DoNothing
        , returning = NumberOfRowsAffected
        }

removeFollowshipStmt :: UserId -> UserId -> Delete Int64
removeFollowshipStmt currentUserId followingUserId = Delete
        { from = userFollowSchema
        , using = pure ()
        , deleteWhere = \_ o -> (fwsUserId o ==. lit currentUserId) &&. (fwsFollowingUserId o ==. lit followingUserId)
        , returning = NumberOfRowsAffected
        }

checkFollowshipStmt :: User -> Expr UserId -> Query (Expr Bool)
checkFollowshipStmt user following = exists $ do
    a <- each userFollowSchema
    where_ $ (fwsUserId a ==. lit (userId user)) &&. (fwsFollowingUserId a ==. following)
