{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Conduit.Core.User where 


import RIO
import qualified Data.Text as T
import Rel8 ( DBType, DBEq )
import Data.Aeson ( FromJSON, ToJSON )
import Crypto.Random
import Crypto.Hash
import Data.ByteString.Base64
import Servant.Server.Experimental.Auth
import Servant

data User = User 
    { userId       :: UserId
    , userName     :: Username
    , userEmail    :: EmailAddress
    , userBio      :: Text
    , userImage    :: Text 
    } deriving (Eq, Show, Read, Generic)

newtype UserId = UserId { getUserId :: Int64 } 
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newtype Username = Username { getUsername :: Text } 
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

newtype EmailAddress = EmailAddress { getEmailAddress :: Text } 
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)

instance ToJSON User
instance FromJSON User

type instance AuthServerData (AuthProtect "Required") = User
type instance AuthServerData (AuthProtect "Optional") = (Maybe User)