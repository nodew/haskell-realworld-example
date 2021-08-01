{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conduit.Api.Tag where

import RIO
import Rel8
import Data.Aeson
import Servant
import Conduit.Db.Schema.Tag

import Conduit.App

newtype TagsResponse = TagsResponse
    { tags :: [Text] } deriving (Generic, ToJSON)

type TagApi = "tags" :> Get '[JSON] TagsResponse

getAllTagsHandler :: RIO AppEnv TagsResponse
getAllTagsHandler = do
    conn <- getConn
    tags <- liftIO $ select conn $ each tagSchema
    return $ TagsResponse $ map entityTagText tags

tagServer :: ServerT TagApi AppM
tagServer = getAllTagsHandler
