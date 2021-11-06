{-# LANGUAGE RankNTypes #-}
module Test.Conduit.Api.AuthApiSpec where

import RIO
import Data.Aeson
import Data.Maybe
import Test.Hspec
import Network.Wai.Test
import Network.HTTP.Types
import Conduit.Api.Common
import Conduit.Api.Auth
import Test.Conduit.TestHelper

spec :: Spec
spec =
     context "Login/Register user" $ afterAll_ cleanUpDb $ do
        withApplication $ do
            it "should register a new user" $ do
                response <- registerNewUser "test001"
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (UserData LoginResponse)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (UserData loginResponse) = fromJust mbBody
                liftIO $ loginRespToken loginResponse `shouldNotBe` ""

            before_ (setupTestUser "test002") $ it "should login and response token" $ do
                response <- loginWith "test002"
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (UserData LoginResponse)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (UserData loginResponse) = fromJust mbBody
                liftIO $ loginRespToken loginResponse `shouldNotBe` ""
