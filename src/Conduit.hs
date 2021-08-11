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
import Data.Default
import Control.Monad.Extra
import GHC.Generics (Generic)
import Servant hiding (runHandler)
import Servant.Server.Experimental.Auth
import Network.Wai (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Crypto.JOSE.JWK hiding (Context)
import Hasql.Pool (acquire)

import Conduit.Config
import Conduit.Auth
import Conduit.Api
import Conduit.Core.User
import Conduit.Db
import Conduit.App

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
    let jwtKey = fromOctets . encodeUtf8 . cfgJwtSecret $ cfg
    pool <- loadPool $ cfgDb cfg
    result <- autoMigrate pool
    whenJust result $ \e -> do
        error $ show e
    let env = AppEnv
                { envDbPool = pool
                , envJwtKey = jwtKey
                }
    runApplication (cfgPort cfg) env

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
