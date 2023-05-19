{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Api where

import Conduit.Api.Article
import Conduit.Api.Auth
import Conduit.Api.Comment
import Conduit.Api.Profile
import Conduit.Api.Tag
import Conduit.Api.User
import Conduit.App
import RIO
import Servant

type ConduitApi =
    "api"
        :> ( AuthApi
                :<|> UserApi
                :<|> ProfileApi
                :<|> ArticleApi
                :<|> CommentApi
                :<|> TagApi
           )

conduitServer :: ServerT ConduitApi AppM
conduitServer =
    authServer
        :<|> userServer
        :<|> profileServer
        :<|> articleServer
        :<|> commentServer
        :<|> tagServer
