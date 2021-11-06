module Test.Conduit.Api.UserApiSpec where

import Test.Hspec
import RIO
import qualified Test.Hspec.Wai as THW
import Test.Conduit.TestHelper
import Conduit.Api.Common
import Conduit.Api.User
import Data.Maybe
import Data.Aeson
import Network.Wai.Test
import Network.HTTP.Types
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    let username = "test"
    context "User APIs"
        $ beforeAll_ (setupTestUser username)
        $ afterAll_ cleanUpDb
        $ withApplication
        $ do
            it "Get user detail with token" $ do
                token <- getAccessToken username
                response <-
                    THW.request
                        methodGet
                        "/api/user"
                        [("Authorization", T.encodeUtf8 $ T.append "Token " token)]
                        ""
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (UserData UserResponse)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (UserData userResponse) = fromJust mbBody
                liftIO $ urUsername userResponse `shouldBe` username

            it "Update user" $ do
                token <- getAccessToken username
                response <-
                    THW.request
                        methodPut
                        "/api/user"
                        [ ("Content-Type", "application/json")
                        , ("Authorization", T.encodeUtf8 $ T.append "Token " token)]
                        "{ \"user\": { \"bio\": \"Test\", \"image\": \"URL\" }}"
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (UserData UserResponse)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (UserData userResponse) = fromJust mbBody
                liftIO $ urUsername userResponse `shouldBe` username
                liftIO $ urImage userResponse `shouldBe` "URL"
                liftIO $ urBio userResponse `shouldBe` "Test"

