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
    { tagId   :: Column f TagId
    , tagText :: Column f Text
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (TagEntity f)

tagSchema :: TableSchema (TagEntity Name)
tagSchema = TableSchema
    { name = "tags"
    , schema = Nothing
    , columns = TagEntity
        { tagId = "tag_id"
        , tagText = "tag_text"
        }
    }

getTagIdStmt :: Text -> Query (Expr TagId)
getTagIdStmt tag = do
    tagEntity <- each tagSchema
    where_ $ tagText tagEntity ==. lit tag
    return $ tagId tagEntity

insertTagStmt :: Text -> Insert [TagId]
insertTagStmt tag = Insert
    { into = tagSchema
    , rows = [TagEntity (unsafeCastExpr $ nextval "tags_tag_id_seq")  (lit tag)]
    , onConflict = DoNothing
    , returning = Projection tagId
    }
