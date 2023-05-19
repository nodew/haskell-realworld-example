{-# LANGUAGE RankNTypes #-}

module Test.Conduit.Api.AuthApiSpec where

import Conduit.Api.Auth
import Conduit.Api.Common
import Data.Aeson
import Data.Maybe
import Network.HTTP.Types
import Network.Wai.Test
import RIO
import Test.Conduit.TestHelper
import Test.Hspec

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

            before_ (setupTestUser "test002") $ after_ (removeTestUser "test002") $ do
                it "should get a list of articles" $ do
                    response <- loginWith "test002"
                    liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                    let mbBody = decode (simpleBody response) :: Maybe (UserData LoginResponse)
                    liftIO $ mbBody `shouldNotBe` Nothing
                    let (UserData loginResponse) = fromJust mbBody
                    liftIO $ loginRespToken loginResponse `shouldNotBe` ""

            it "should not login and throw error" $ do
                response <- loginWith "test002"
                liftIO $ statusCode (simpleStatus response) `shouldBe` 401
                let mbBody = decode (simpleBody response) :: Maybe (UserData LoginResponse)
                liftIO $ mbBody `shouldBe` Nothing
