{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Api.Auth where

import Conduit.Api.Common
import Conduit.App
import Conduit.Core.Password
import Conduit.Core.User
import Conduit.Environment
import Conduit.JWT
import qualified Conduit.Repository.User as UserRepository
import Conduit.Util
import Data.Aeson
import Data.Aeson.Types
import RIO
import Servant

data LoginUser = LoginUser
    { loginEmail :: Text
    , loginPassword :: Text
    }
    deriving (Show, Generic)

instance ToJSON LoginUser where
    toJSON = genericToJSON $ toJsonOptions 5

instance FromJSON LoginUser where
    parseJSON = genericParseJSON $ toJsonOptions 5

data NewUser = NewUser
    { newUserUsername :: Text
    , newUserEmail :: Text
    , newUserPassword :: Text
    }
    deriving (Show, Generic)

instance ToJSON NewUser where
    toJSON = genericToJSON $ toJsonOptions 7

instance FromJSON NewUser where
    parseJSON = genericParseJSON $ toJsonOptions 7

data LoginResponse = LoginResponse
    { loginRespUsername :: Text
    , loginRespEmail :: Text
    , loginRespToken :: Text
    , loginRespBio :: Text
    , loginRespImage :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON LoginResponse where
    toJSON = genericToJSON $ toJsonOptions 9

instance FromJSON LoginResponse where
    parseJSON = genericParseJSON $ toJsonOptions 9

mapUserToLoginResponse :: User -> Text -> LoginResponse
mapUserToLoginResponse user token =
    LoginResponse
        { loginRespUsername = getUsername $ userName user
        , loginRespEmail = getEmailAddress $ userEmail user
        , loginRespToken = token
        , loginRespBio = userBio user
        , loginRespImage = userImage user
        }

{---------------------------------------------------------------------------------------}

type AuthApi =
    "users"
        :> "login"
        :> ReqBody '[JSON] (UserData LoginUser)
        :> Post '[JSON] (UserData LoginResponse)
        :<|> "users"
            :> ReqBody '[JSON] (UserData NewUser)
            :> Post '[JSON] (UserData LoginResponse)

loginHandler :: UserData LoginUser -> AppM (UserData LoginResponse)
loginHandler (UserData u) =
    UserRepository.getUserByEmailAndPassword (EmailAddress $ loginEmail u) (Password $ loginPassword u)
        >>= maybe (throwIO err401) (fmap UserData . genUserResponse)

registerHandler :: UserData NewUser -> AppM (UserData LoginResponse)
registerHandler (UserData u) =
    UserRepository.saveNewUser newUser password
        >>= maybe (throwIO err400) (fmap UserData . genUserResponse)
    where
        username = Username $ newUserUsername u
        email = EmailAddress $ newUserEmail u
        password = Password $ newUserPassword u
        newUser = User (UserId 0) username email "" ""

genUserResponse :: User -> AppM LoginResponse
genUserResponse user = do
    let username = userName user
    jwtKey <- getJwtKey'
    claims <- liftIO $ mkClaims username
    liftIO $
        signJwt jwtKey claims
            >>= either (\_ -> throwIO err422) (return . mapUserToLoginResponse user)

authServer :: ServerT AuthApi AppM
authServer = loginHandler :<|> registerHandler
