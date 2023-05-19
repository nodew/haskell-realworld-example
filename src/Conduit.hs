{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Conduit where

import Conduit.Api
import Conduit.App
import Conduit.Auth
import Conduit.Config
import Conduit.Core.User
import Conduit.Db
import Control.Monad.Extra
import Crypto.JOSE.JWK hiding (Context)
import Data.Default
import GHC.Generics (Generic)
import Hasql.Pool (acquire)
import Network.Wai (Middleware, Request, requestHeaders)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import RIO
import Servant hiding (runHandler)
import Servant.Server.Experimental.Auth
import Prelude (putStrLn)

type AuthContext = AuthHandler Request User ': AuthHandler Request (Maybe User) ': '[]

api :: Proxy ConduitApi
api = Proxy

mkApp :: AppEnv -> Application
mkApp env =
    serveWithContext api serverAuthContext hoistedServer
    where
        serverAuthContext :: Context AuthContext
        serverAuthContext = handleAuthentication env :. handleOptionalAuthentication env :. EmptyContext

        hoistedServer :: ServerT ConduitApi Servant.Handler
        hoistedServer = hoistServerWithContext api (Proxy :: Proxy AuthContext) (runHandler env) conduitServer

runConduit :: Config -> IO ()
runConduit cfg = do
    let jwtKey = fromOctets . encodeUtf8 . cfgJwk $ cfg
    pool <- loadPool (cfgConnectString cfg) (cfgPoolSize cfg)
    result <- autoMigrate pool
    whenJust result $ \e -> do
        error $ show e
    let env =
            AppEnv
                { envDbPool = pool
                , envJwtKey = jwtKey
                }
    runApplication (cfgPort cfg) env

runApplication :: Int -> AppEnv -> IO ()
runApplication port env = do
    warpLogger <- jsonRequestLogger
    let warpSettings =
            Warp.defaultSettings
                & Warp.setPort port
                & Warp.setTimeout 60
    Warp.runSettings warpSettings $ warpLogger $ mkApp env

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
    mkRequestLogger $ def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}
