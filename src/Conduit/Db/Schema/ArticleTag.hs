{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Schema.ArticleTag where

import RIO
import Rel8

import Conduit.App
import Conduit.Core.Article

data ArticleTagEntity f = ArticleTagEntity
    { tgdArticleId :: Column f ArticleId  
    , tgdTagId     :: Column f TagId 
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (ArticleTagEntity f)

articleTagSchema :: TableSchema (ArticleTagEntity Name)
articleTagSchema = TableSchema
    { name = "tagged"
    , schema = Nothing
    , columns = ArticleTagEntity
        { tgdArticleId = "tgd_article_id"
        , tgdTagId = "tgd_tag_id"
        }
    }
