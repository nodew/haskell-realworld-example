{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}

module Conduit.Db.Schema.User where

import Conduit.Core.Password
import Conduit.Core.User
import Hasql.Connection (Connection)
import RIO hiding (set)
import Rel8

data UserEntity f = UserEntity
    { entityUserId :: Column f UserId
    , entityUserName :: Column f Username
    , entityUserEmail :: Column f EmailAddress
    , entityUserPassword :: Column f HashedPassword
    , entityUserBio :: Column f Text
    , entityUserImage :: Column f Text
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (UserEntity f)

userSchema :: TableSchema (UserEntity Name)
userSchema =
    TableSchema
        { name = "users"
        , schema = Nothing
        , columns =
            UserEntity
                { entityUserId = "user_id"
                , entityUserName = "user_username"
                , entityUserEmail = "user_email"
                , entityUserPassword = "user_password"
                , entityUserBio = "user_bio"
                , entityUserImage = "user_image"
                }
        }

mapUserEntityToUser :: UserEntity Result -> User
mapUserEntityToUser entity =
    User
        { userId = entityUserId entity
        , userName = entityUserName entity
        , userEmail = entityUserEmail entity
        , userBio = entityUserBio entity
        , userImage = entityUserImage entity
        }

updateUserProperties :: User -> UserEntity Expr -> UserEntity Expr
updateUserProperties user expr =
    expr
        { entityUserName = lit (userName user)
        , entityUserEmail = lit (userEmail user)
        , entityUserBio = lit (userBio user)
        , entityUserImage = lit (userImage user)
        }

updatePassword :: HashedPassword -> UserEntity Expr -> UserEntity Expr
updatePassword hash expr =
    expr
        { entityUserPassword = lit hash
        }

getUserByIdStmt :: Expr UserId -> Query (UserEntity Expr)
getUserByIdStmt uid = do
    a <- each userSchema
    where_ $ entityUserId a ==. uid
    return a

getUserByNameStmt :: Username -> Query (UserEntity Expr)
getUserByNameStmt name = do
    a <- each userSchema
    where_ $ entityUserName a ==. lit name
    return a

getUserByEmailStmt :: EmailAddress -> Query (UserEntity Expr)
getUserByEmailStmt email = do
    a <- each userSchema
    where_ $ entityUserEmail a ==. lit email
    return a

insertUserStmt :: User -> HashedPassword -> Insert [UserId]
insertUserStmt user hash =
    Insert
        { into = userSchema
        , rows =
            values
                [ UserEntity
                    { entityUserId = unsafeCastExpr $ nextval "users_user_id_seq"
                    , entityUserName = lit (userName user)
                    , entityUserEmail = lit (userEmail user)
                    , entityUserBio = lit (userBio user)
                    , entityUserImage = lit (userImage user)
                    , entityUserPassword = lit hash
                    }
                ]
        , onConflict = DoNothing
        , returning = Projection entityUserId
        }

updateUserStmt :: User -> Maybe HashedPassword -> Update Int64
updateUserStmt user mbPassword =
    Update
        { target = userSchema
        , from = pure ()
        , updateWhere = \_ o -> entityUserId o ==. lit (userId user)
        , set = setter
        , returning = NumberOfRowsAffected
        }
    where
        setter _ = case mbPassword of
            Just password -> updatePassword password . updateUserProperties user
            _ -> updateUserProperties user
