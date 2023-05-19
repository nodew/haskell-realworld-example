{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Conduit.Db.Schema.ArticleTag where

import Conduit.Core.Article
import RIO
import Rel8

data ArticleTagEntity f = ArticleTagEntity
    { tgdArticleId :: Column f ArticleId
    , tgdTagId :: Column f TagId
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (ArticleTagEntity f)

articleTagSchema :: TableSchema (ArticleTagEntity Name)
articleTagSchema =
    TableSchema
        { name = "tagged"
        , schema = Nothing
        , columns =
            ArticleTagEntity
                { tgdArticleId = "tgd_article_id"
                , tgdTagId = "tgd_tag_id"
                }
        }

insertArticleTagsStmt :: ArticleId -> [TagId] -> Insert Int64
insertArticleTagsStmt articleId tagIds =
    Insert
        { into = articleTagSchema
        , rows = values $ map (ArticleTagEntity (lit articleId) . lit) tagIds
        , onConflict = DoNothing
        , returning = NumberOfRowsAffected
        }

getAllArticleTagsStmt :: Expr ArticleId -> Query (Expr TagId)
getAllArticleTagsStmt articleId = do
    articleTag <- each articleTagSchema
    where_ $ tgdArticleId articleTag ==. articleId
    return $ tgdTagId articleTag

deleteAllArticleTagsStmt :: ArticleId -> Delete Int64
deleteAllArticleTagsStmt articleId' =
    Delete
        { from = articleTagSchema
        , using = pure ()
        , deleteWhere = \_ row -> tgdArticleId row ==. lit articleId'
        , returning = NumberOfRowsAffected
        }

deleteArticleTagStmt :: ArticleId -> TagId -> Delete Int64
deleteArticleTagStmt articleId tagId =
    Delete
        { from = articleTagSchema
        , using = pure ()
        , deleteWhere = \_ row -> tgdArticleId row ==. lit articleId &&. tgdTagId row ==. lit tagId
        , returning = NumberOfRowsAffected
        }
