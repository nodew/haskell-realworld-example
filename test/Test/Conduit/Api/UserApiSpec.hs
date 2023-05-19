module Test.Conduit.Api.UserApiSpec where

import Conduit.Api.Common
import Conduit.Api.User
import Data.Aeson
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Network.Wai.Test
import RIO
import Test.Conduit.TestHelper
import Test.Hspec
import qualified Test.Hspec.Wai as THW

spec :: Spec
spec = do
    let username = "test"
    context "User APIs" $
        beforeAll_ (setupTestUser username) $
            afterAll_ cleanUpDb $
                withApplication $
                    do
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
                                    , ("Authorization", T.encodeUtf8 $ T.append "Token " token)
                                    ]
                                    "{ \"user\": { \"bio\": \"Test\", \"image\": \"URL\" }}"
                            liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                            let mbBody = decode (simpleBody response) :: Maybe (UserData UserResponse)
                            liftIO $ mbBody `shouldNotBe` Nothing
                            let (UserData userResponse) = fromJust mbBody
                            liftIO $ urUsername userResponse `shouldBe` username
                            liftIO $ urImage userResponse `shouldBe` "URL"
                            liftIO $ urBio userResponse `shouldBe` "Test"
