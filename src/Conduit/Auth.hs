{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Conduit.Auth where

import RIO
import RIO.ByteString as B (stripPrefix)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Crypto.JOSE.JWK (JWK)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Text as T
import Hasql.Connection (Connection)
import Network.Wai (Request, requestHeaders)
import qualified RIO.Text as T
import Servant (AuthProtect, err401)
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    AuthServerData,
    mkAuthHandler,
  )

import Conduit.App (AppEnv (envConn, envJwtKey))
import Conduit.Core.User (User (..), Username (..))
import Conduit.Db.User (getUserByName)
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
          _ -> liftIO $ throwIO err401 
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