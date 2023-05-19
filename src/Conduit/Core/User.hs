{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Conduit.Core.User where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import RIO
import Rel8 (DBEq, DBType)
import Servant
import Servant.Server.Experimental.Auth

data User = User
    { userId :: UserId
    , userName :: Username
    , userEmail :: EmailAddress
    , userBio :: Text
    , userImage :: Text
    }
    deriving (Eq, Show, Read, Generic)

newtype UserId = UserId {getUserId :: Int64}
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newtype Username = Username {getUsername :: Text}
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newtype EmailAddress = EmailAddress {getEmailAddress :: Text}
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

instance ToJSON User

instance FromJSON User

type instance AuthServerData (AuthProtect "Required") = User

type instance AuthServerData (AuthProtect "Optional") = (Maybe User)
