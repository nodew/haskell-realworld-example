module Conduit.TestHelper where

import RIO
import Dhall
import Crypto.JOSE.JWK
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Test.Hspec
import Test.Hspec.Wai
import Hasql.Connection

import Conduit
import Conduit.App
import Conduit.Config

loadTestEnv :: IO AppEnv
loadTestEnv = do
    cfg <- input auto "./conduit-test.dhall"
    let postgresSettings = mapDbConfigToSettings $ cfgDb cfg
    let jwtKey = fromOctets . encodeUtf8 . cfgJwtSecret $ cfg
    conn <- acquire postgresSettings
    case conn of
        Right _conn -> do
            let env = AppEnv
                        { envConn = _conn
                        , envJwtKey = jwtKey
                        }
            return env
        _ ->
            error "Failed to connect to database"

loadApplication :: IO Application
loadApplication = do
    mkApp <$> loadTestEnv

withApplication :: SpecWith ((), Application) -> Spec
withApplication = with loadApplication
