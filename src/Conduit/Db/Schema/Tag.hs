{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Schema.Tag where

import RIO
import Rel8

import Conduit.Core.Article

data TagEntity f = TagEntity
    { entityTagId   :: Column f TagId
    , entityTagText :: Column f Text
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (TagEntity f)

tagSchema :: TableSchema (TagEntity Name)
tagSchema = TableSchema
    { name = "tags"
    , schema = Nothing
    , columns = TagEntity
        { entityTagId = "tag_id"
        , entityTagText = "tag_text"
        }
    }

getTagIdStmt :: Text -> Query (Expr TagId)
getTagIdStmt tag = do
    tagEntity <- each tagSchema
    where_ $ entityTagText tagEntity ==. lit tag
    return $ entityTagId tagEntity

insertTagStmt :: Text -> Insert [TagId]
insertTagStmt tag = Insert
    { into = tagSchema
    , rows = values [TagEntity (unsafeCastExpr $ nextval "tags_tag_id_seq")  (lit tag)]
    , onConflict = Abort
    , returning = Projection entityTagId
    }
