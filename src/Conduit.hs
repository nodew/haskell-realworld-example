{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Conduit where

import RIO
import Prelude (putStrLn)
import Conduit.App
import Data.Aeson
import Data.Default
import GHC.Generics (Generic)
import Servant hiding (runHandler)
import Servant.Server.Experimental.Auth
import Network.Wai (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Crypto.JOSE.JWK hiding (Context)
import Hasql.Connection

import Conduit.Config
import Conduit.Auth
import Conduit.Api
import Conduit.Core.User

type AuthContext = AuthHandler Request User ': AuthHandler Request (Maybe User) ': '[]

api :: Proxy ConduitApi
api = Proxy

mkApp :: AppEnv -> Application
mkApp env =
    serveWithContext api serverAuthContext hoistedServer
    where
        serverAuthContext :: Context AuthContext
        serverAuthContext = handleAuthentication env :. handleOptionalAuthentication env :. EmptyContext

        hoistedServer = hoistServerWithContext api (Proxy :: Proxy AuthContext) (runHandler env) conduitServer

runConduit :: Config -> IO ()
runConduit cfg = do
    let postgresSettings = mapDbConfigToSettings $ cfgDb cfg
    let jwtKey = fromOctets . encodeUtf8 . cfgJwtSecret $ cfg
    conn <- acquire postgresSettings
    case conn of
        Right _conn -> do
            let env = AppEnv 
                        { envConn = _conn
                        , envJwtKey = jwtKey 
                        }
            runApplication (cfgPort cfg) env
        _ ->
            putStrLn "Failed to connect to database"

runApplication :: Word16 -> AppEnv -> IO ()
runApplication port env = do
    warpLogger <- jsonRequestLogger
    let warpSettings = Warp.defaultSettings
                     & Warp.setPort (fromIntegral port)
                     & Warp.setTimeout 60
    Warp.runSettings warpSettings $ warpLogger $ mkApp env

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
    mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
