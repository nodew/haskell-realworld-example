{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Conduit.Api.Tag where

import RIO
import Rel8
import Data.Aeson
import Hasql.Transaction (statement)
import Servant


import Conduit.Db.Schema.Tag
import Conduit.Db.Helper

import Conduit.App

newtype TagsResponse = TagsResponse
    { tags :: [Text] } deriving (Generic, ToJSON)

type TagApi = "tags" :> Get '[JSON] TagsResponse

getAllTagsHandler :: AppM TagsResponse
getAllTagsHandler = withTransaction $ \transaction -> do
    tags <- transaction $ statement () $ select $ each tagSchema
    return $ TagsResponse $ map entityTagText tags

tagServer :: ServerT TagApi AppM
tagServer = getAllTagsHandler
