{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Conduit.Auth where

import RIO
import RIO.ByteString as B (stripPrefix)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Crypto.JOSE.JWK (JWK)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Text as T
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, err401, errBody, throwError)
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    mkAuthHandler,
  )

import Conduit.App (AppEnv (envJwtKey))
import Conduit.Core.User (User (..), Username (..))
import Conduit.Repository.User (getUserByName)
import Conduit.JWT (getSubject, verifyJwt)
import Conduit.Util ( hoistMaybe )

handleOptionalAuthentication :: AppEnv -> AuthHandler Request (Maybe User)
handleOptionalAuthentication env =
  let handler req = liftIO $ getUserFromJwtToken env req
   in mkAuthHandler handler

handleAuthentication :: AppEnv -> AuthHandler Request User
handleAuthentication env =
  let handler req = do
        user <- liftIO $ getUserFromJwtToken env req
        case user of
          Just user' -> return user'
          _ -> throwError $ err401 { errBody = "Invalid JWT Token" }
   in mkAuthHandler handler

getUserFromJwtToken :: AppEnv -> Request -> IO (Maybe User)
getUserFromJwtToken env req = runMaybeT $ do
  token <- hoistMaybe $ lookup "Authorization" (requestHeaders req)
  username <- decodeToken (envJwtKey env) token
  MaybeT $ liftIO $ runRIO env $ getUserByName username

decodeToken :: JWK -> ByteString -> MaybeT IO Username
decodeToken jwk authToken = do
  token <- hoistMaybe $ B.stripPrefix "Token " authToken
  claimSet <- MaybeT $ eitherToMaybe <$> verifyJwt jwk token
  subject <- hoistMaybe $ getSubject claimSet
  if T.length subject > 0
    then return $ Username subject
    else hoistMaybe Nothing
