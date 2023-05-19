{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Api.Tag where

import Conduit.App
import Conduit.Db
import Data.Aeson
import Hasql.Transaction (statement)
import RIO
import Rel8
import Servant

newtype TagsResponse = TagsResponse
    {tags :: [Text]}
    deriving (Generic)

instance ToJSON TagsResponse where
    toJSON (TagsResponse a) = object ["tags" .= a]

type TagApi = "tags" :> Get '[JSON] TagsResponse

getAllTagsHandler :: AppM TagsResponse
getAllTagsHandler = do
    tags <- executeStmt $ select $ each tagSchema
    return $ TagsResponse $ map entityTagText tags

tagServer :: ServerT TagApi AppM
tagServer = getAllTagsHandler
