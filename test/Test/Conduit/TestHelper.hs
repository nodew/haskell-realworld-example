{-# LANGUAGE RankNTypes #-}

module Test.Conduit.TestHelper where

import Conduit
import Conduit.Api.Auth
import Conduit.Api.Common
import Conduit.App
import Conduit.Config
import Conduit.Core.Password
import Conduit.Db
import Control.Monad.Extra
import Crypto.JOSE.JWK
import Data.Aeson
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Dhall
import Hasql.Transaction
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Test
import RIO
import Test.Hspec
import Test.Hspec.Wai
import qualified Test.Hspec.Wai as THW
import System.IO (putStrLn)

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
loadApplication = mkApp <$> loadTestEnv

withApplication :: SpecWith ((), Application) -> Spec
withApplication = with loadApplication

cleanUpDb :: IO ()
cleanUpDb = do
    env <- loadTestEnv
    runTransactionWithPool (envDbPool env) $
        truncateTables
            [ "follows"
            , "tagged"
            , "comments"
            , "favorited"
            , "tags"
            , "articles"
            , "users"
            ]

setupSeedData :: Transaction () -> IO ()
setupSeedData transaction = do
    env <- loadTestEnv
    runTransactionWithPool (envDbPool env) transaction

setupTestUser :: Text -> IO ()
setupTestUser username = do
    let email = T.append username "@test.com"
    let password = Password $ T.append username "password"
    (hash, salt) <- hashPassword password
    setupSeedData $
        sql $ mconcat
            [ "INSERT INTO users (user_email, user_username, user_password, user_salt, user_bio, user_image)"
            , "VALUES ('"
            , T.encodeUtf8 email
            , "', '"
            , T.encodeUtf8 username
            , "', '"
            , T.encodeUtf8 . getHashedPasswd $ hash
            , "', '"
            , T.encodeUtf8 . getSalt $ salt
            , "', ''"
            , ", '');"
            ]

registerNewUser :: forall st. Text -> THW.WaiSession st SResponse
registerNewUser username = do
    THW.request
        methodPost
        "/api/users"
        [("Content-Type", "application/json")]
        registerRequestData
    where
        email = T.append username "@test.com"
        password = T.append username "password"
        newUser = NewUser username email password
        registerRequestData = encode $ UserData newUser

loginWith :: forall st. Text -> THW.WaiSession st SResponse
loginWith username =
    THW.request
        methodPost
        "/api/users/login"
        [("Content-Type", "application/json")]
        loginRequestData
    where
        email = T.append username "@test.com"
        password = T.append username "password"
        loginUser = LoginUser email password
        loginRequestData = encode $ UserData loginUser

getAccessToken :: forall st. Text -> THW.WaiSession st Text
getAccessToken username = do
    response <- loginWith username
    let mbBody = decode (simpleBody response) :: Maybe (UserData LoginResponse)
    return $ case mbBody of
        Just (UserData loginResponse) -> loginRespToken loginResponse
        _ -> ""
