{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Conduit.Db.Schema.Article where

import Conduit.Core.Article
import Conduit.Core.User
import Data.Time
import RIO
import Rel8
import Rel8.Expr.Time

data ArticleEntity f = ArticleEntity
    { entityArticleId :: Column f ArticleId
    , entityArticleAuthorId :: Column f UserId
    , entityArticleTitle :: Column f Text
    , entityArticleSlug :: Column f Slug
    , entityArticleDescription :: Column f Text
    , entityArticleBody :: Column f Text
    , entityArticleCreatedAt :: Column f UTCTime
    , entityArticleUpdatedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (ArticleEntity f)

articleSchema :: TableSchema (ArticleEntity Name)
articleSchema =
    TableSchema
        { name = "articles"
        , schema = Nothing
        , columns =
            ArticleEntity
                { entityArticleId = "article_id"
                , entityArticleAuthorId = "article_user_id"
                , entityArticleTitle = "article_title"
                , entityArticleSlug = "article_slug"
                , entityArticleDescription = "article_description"
                , entityArticleBody = "article_body"
                , entityArticleCreatedAt = "article_createdat"
                , entityArticleUpdatedAt = "article_updatedat"
                }
        }

mapArticleEntityToArticle :: ArticleEntity Result -> Article
mapArticleEntityToArticle entity =
    Article
        { articleId = entityArticleId entity
        , articleAuthorId = entityArticleAuthorId entity
        , articleTitle = entityArticleTitle entity
        , articleSlug = entityArticleSlug entity
        , articleDescription = entityArticleDescription entity
        , articleBody = entityArticleBody entity
        , articleTags = []
        , articleCreatedAt = entityArticleCreatedAt entity
        , articleUpdatedAt = entityArticleUpdatedAt entity
        }

insertArticleStmt :: Article -> Insert [ArticleId]
insertArticleStmt article =
    Insert
        { into = articleSchema
        , rows =
            values
                [ ArticleEntity
                    { entityArticleId = unsafeCastExpr $ nextval "articles_article_id_seq"
                    , entityArticleAuthorId = lit (articleAuthorId article)
                    , entityArticleTitle = lit (articleTitle article)
                    , entityArticleSlug = lit (articleSlug article)
                    , entityArticleDescription = lit (articleDescription article)
                    , entityArticleBody = lit (articleBody article)
                    , entityArticleCreatedAt = now
                    , entityArticleUpdatedAt = now
                    }
                ]
        , onConflict = Abort
        , returning = Projection entityArticleId
        }

updateArticleStmt :: Article -> Update Int64
updateArticleStmt article =
    Update
        { target = articleSchema
        , from = pure ()
        , updateWhere = \_ row -> entityArticleId row ==. lit (articleId article)
        , set = \_ row ->
            row
                { entityArticleTitle = lit (articleTitle article)
                , entityArticleDescription = lit (articleDescription article)
                , entityArticleBody = lit (articleBody article)
                , entityArticleUpdatedAt = now
                }
        , returning = NumberOfRowsAffected
        }

getArticleEntityByIdStmt :: Expr ArticleId -> Query (ArticleEntity Expr)
getArticleEntityByIdStmt id = do
    article <- each articleSchema
    where_ $ entityArticleId article ==. id
    return article

getArticleEntityBySlugStmt :: Expr Slug -> Query (ArticleEntity Expr)
getArticleEntityBySlugStmt slug = do
    article <- each articleSchema
    where_ $ entityArticleSlug article ==. slug
    return article

deleteArticleByIdStmt :: ArticleId -> Delete Int64
deleteArticleByIdStmt articleId' =
    Delete
        { from = articleSchema
        , using = pure ()
        , deleteWhere = \_ row -> entityArticleId row ==. lit articleId'
        , returning = NumberOfRowsAffected
        }
