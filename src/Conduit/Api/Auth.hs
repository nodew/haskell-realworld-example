{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Conduit.Api.Auth where

import RIO
import Data.Aeson
import Data.Aeson.Types
import Servant
import Prelude (putStrLn)

import Conduit.App
import Conduit.Api.Common
import qualified Conduit.Db.User as UserDb
import Conduit.Core.Password
import Conduit.Core.User
import Conduit.JWT
import Conduit.Util

data LoginUser = LoginUser
    { loginEmail :: Text
    , loginPassword :: Text
    } deriving (Show, Generic)

instance ToJSON LoginUser where
    toJSON = genericToJSON $ toJsonOptions 5

instance FromJSON LoginUser where
    parseJSON = genericParseJSON $ toJsonOptions 5

data NewUser = NewUser
    { newUserUsername :: Text
    , newUserEmail    :: Text
    , newUserPassword :: Text
    } deriving (Show, Generic)

instance ToJSON NewUser where
    toJSON = genericToJSON $ toJsonOptions 7

instance FromJSON NewUser where
    parseJSON = genericParseJSON $ toJsonOptions 7

data LoginResponse = LoginResponse
    { loginRespUsername :: Text
    , loginRespEmail    :: Text
    , loginRespToken    :: Text
    , loginRespBio      :: Text
    , loginRespImage    :: Text
    } deriving (Show, Generic)

instance ToJSON LoginResponse where
    toJSON = genericToJSON $ toJsonOptions 9

mapUserToLoginResponse :: User -> Text -> LoginResponse
mapUserToLoginResponse user token = LoginResponse
    { loginRespUsername = getUsername $ userName user
    , loginRespEmail    = getEmailAddress $ userEmail user
    , loginRespToken    = token
    , loginRespBio      = userBio user
    , loginRespImage    = userImage user
    }

{---------------------------------------------------------------------------------------}

type AuthApi = "users"
                    :> "login"
                    :> ReqBody '[JSON] (UserData LoginUser)
                    :> Post '[JSON] (UserData LoginResponse)
           :<|> "users"
                    :> ReqBody '[JSON] (UserData NewUser)
                    :> Post '[JSON] (UserData LoginResponse)

loginHandler :: UserData LoginUser -> AppM (UserData LoginResponse)
loginHandler (UserData u) = do
    result <- UserDb.getUserByEmailAndPassword (EmailAddress $ loginEmail u) (Password $ loginPassword u)
    case result of
        Nothing ->
            throwIO err401
        Just user -> UserData <$> genUserResponse user

registerHandler :: UserData NewUser -> AppM (UserData LoginResponse)
registerHandler (UserData u) = do
    result <- UserDb.saveNewUser newUser password
    case result of
        Nothing -> throwIO err401
        Just user -> UserData <$> genUserResponse user
    where
        username = Username $ newUserUsername u
        email = EmailAddress $ newUserEmail u
        password = Password $ newUserPassword u
        newUser = User (UserId 0) username email "" ""

genUserResponse :: User -> AppM LoginResponse
genUserResponse user = do
    let username = userName user
    jwtKey <- getJwtKey
    claims <- liftIO $ mkClaims username
    signedToken <- liftIO $ signJwt jwtKey claims
    case signedToken of
        Left _ -> throwIO err422
        Right token -> return $ mapUserToLoginResponse user token

authServer :: ServerT AuthApi AppM
authServer = loginHandler :<|> registerHandler