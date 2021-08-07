{-# LANGUAGE RankNTypes #-}
module Conduit.Api.AuthApiSpec where

import RIO
import Data.Aeson
import Data.Maybe
import Test.Hspec
import Test.Hspec.Wai (get, liftIO, matchHeaders, matchStatus, shouldRespondWith,
                 with, (<:>))
import qualified Test.Hspec.Wai as THW
import Network.Wai.Test
import Network.HTTP.Types
import qualified Data.Text as T

import Conduit
import Conduit.App
import Conduit.Config
import Conduit.TestHelper
import Conduit.Api.Common
import Conduit.Api.Auth

registerNewUser :: forall st . Text -> THW.WaiSession st SResponse
registerNewUser username =
    THW.request
        methodPost
        "/api/users"
        [("Content-Type", "application/json")]
        registerRequestData
    where
        newUser = NewUser username (T.append username "@test.com") (T.append username "password")
        registerRequestData = encode $ UserData newUser

loginWith :: forall st . Text -> THW.WaiSession st SResponse
loginWith username =
    THW.request
        methodPost
        "/api/users/login"
        [("Content-Type", "application/json")]
        loginRequestData
    where
        loginUser = LoginUser (T.append username "@test.com") (T.append username "password")
        loginRequestData = encode $ UserData loginUser

spec :: Spec
spec =
     context "Login/Register user" $ do
        withApplication $ do
            it "should register a new user" $ do
                response <- registerNewUser "test001"
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (UserData LoginResponse)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (UserData loginResponse) = fromJust mbBody
                liftIO $ loginRespToken loginResponse `shouldNotBe` ""

            it "should login and response token" $ do
                _ <- registerNewUser "test002"
                response <- loginWith "test002"
                liftIO $ statusCode (simpleStatus response) `shouldBe` 200
                let mbBody = decode (simpleBody response) :: Maybe (UserData LoginResponse)
                liftIO $ mbBody `shouldNotBe` Nothing
                let (UserData loginResponse) = fromJust mbBody
                liftIO $ loginRespToken loginResponse `shouldNotBe` ""

