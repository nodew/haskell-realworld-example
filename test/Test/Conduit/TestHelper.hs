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
import Hasql.Transaction
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Test
import RIO
import System.IO (putStrLn)
import Test.Hspec
import Test.Hspec.Wai
import qualified Test.Hspec.Wai as THW

loadTestEnv :: IO AppEnv
loadTestEnv = do
    cfg <- loadConfigFromEnv
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

runSql :: Transaction () -> IO ()
runSql transaction = do
    env <- loadTestEnv
    runTransactionWithPool (envDbPool env) transaction

setupTestArticle :: Text -> IO ()
setupTestArticle userId = do
    let articleSlug = "test-article"
    let articleTitle = "test article title"
    let articleDescription = "test article description"
    let articleBody = "test article body"
    let articleUserId = userId
    runSql $
        sql $
            mconcat
                [ "INSERT INTO articles (article_slug, article_title, article_description, article_body, article_user_id)"
                , "VALUES ('"
                , T.encodeUtf8 articleSlug
                , "', '"
                , T.encodeUtf8 articleTitle
                , "', '"
                , T.encodeUtf8 articleDescription
                , "', '"
                , T.encodeUtf8 articleBody
                , "', "
                , T.encodeUtf8 articleUserId
                , ");"
                ]

setupTestUser :: Text -> IO ()
setupTestUser username = do
    let email = T.append username "@test.com"
    let password = Password $ T.append username "password"
    hash <- hashPassword password
    runSql $
        sql $
            mconcat
                [ "INSERT INTO users (user_email, user_username, user_password, user_bio, user_image)"
                , "VALUES ('"
                , T.encodeUtf8 email
                , "', '"
                , T.encodeUtf8 username
                , "', '"
                , T.encodeUtf8 . getHashedPasswd $ hash
                , "', ''"
                , ", '');"
                ]

removeTestUser :: Text -> IO ()
removeTestUser username = do
    let email = T.append username "@test.com"
    let password = Password $ T.append username "password"
    hash <- hashPassword password
    runSql $
        sql $
            mconcat
                [ "DELETE FROM users "
                , "WHERE user_username = '"
                , T.encodeUtf8 username
                , "';"
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
