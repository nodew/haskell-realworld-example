module Test.Conduit.TestHelper where

import RIO
import Control.Monad.Extra
import Dhall
import Crypto.JOSE.JWK
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Test.Hspec
import Test.Hspec.Wai
import Hasql.Pool

import Conduit
import Conduit.App
import Conduit.Config
import Conduit.Db

loadTestEnv :: IO AppEnv
loadTestEnv = do
    cfg <- input auto "./conduit-test.dhall"
    let jwtKey = fromOctets . encodeUtf8 . cfgJwtSecret $ cfg
    pool <- loadPool $ cfgDb cfg
    result <- autoMigrate pool
    whenJust result $ \e -> do
        error $ show e
    let env = AppEnv
                { envDbPool = pool
                , envJwtKey = jwtKey
                }
    return env

loadApplication :: IO Application
loadApplication = do
    mkApp <$> loadTestEnv

withApplication :: SpecWith ((), Application) -> Spec
withApplication = with loadApplication

cleanUpDb :: IO ()
cleanUpDb = do
    env <- loadTestEnv
    runTransactionWithPool (envDbPool env) $ truncateTables
        [ "follows"
        , "tagged"
        , "comments"
        , "favorited"
        , "tags"
        , "articles"
        , "users"
        ]
