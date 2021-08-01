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
import Conduit.Api.Article
import Conduit.Api.Comment
import Conduit.Api.Tag

type ConduitApi = "api" :> ( AuthApi
                        :<|> UserApi
                        :<|> ProfileApi
                        :<|> ArticleApi
                        :<|> CommentApi
                        :<|> TagApi
                        )

conduitServer :: ServerT ConduitApi AppM
conduitServer = authServer
            :<|> userServer
            :<|> profileServer
            :<|> articleServer
            :<|> commentServer
            :<|> tagServer
