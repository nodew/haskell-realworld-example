{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Conduit.Api where

import RIO
import Servant

import Conduit.App
import Conduit.Api.Auth
import Conduit.Api.User
import Conduit.Api.Profile

type ConduitApi = "api" :> (AuthApi 
                        :<|> UserApi
                        :<|> ProfileApi)

conduitServer :: ServerT ConduitApi AppM
conduitServer = authServer 
            :<|> userServer
            :<|> profileServer  
