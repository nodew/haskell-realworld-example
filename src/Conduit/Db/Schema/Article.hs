{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Schema.Article where

import RIO
import Rel8
import Data.Time

import Conduit.Core.User
import Conduit.Core.Article

data ArticleEntity f = ArticleEntity
    { entityArticleId          :: Column f ArticleId
    , entityArticleAuthorId    :: Column f UserId
    , entityArticleTitle       :: Column f Text
    , entityArticleSlug        :: Column f Slug
    , entityArticleDescription :: Column f Text
    , entityArticleBody        :: Column f Text
    , entityArticleCreatedAt   :: Column f UTCTime
    , entityArticleUpdatedAt   :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (ArticleEntity f)

articleSchema :: TableSchema (ArticleEntity Name)
articleSchema = TableSchema
    { name = "articles"
    , schema = Nothing
    , columns = ArticleEntity
        { entityArticleId          = "article_id"
        , entityArticleAuthorId    = "article_user_id"
        , entityArticleTitle       = "article_title"
        , entityArticleSlug        = "article_slug"
        , entityArticleDescription = "article_description"
        , entityArticleBody        = "article_body"
        , entityArticleCreatedAt   = "article_createdat"
        , entityArticleUpdatedAt   = "article_updatedat"
        }
    }

mapArticleEntityToArticle :: ArticleEntity Result -> Article
mapArticleEntityToArticle entity = Article
    { articleId          = entityArticleId entity
    , articleAuthorId    = entityArticleAuthorId entity
    , articleTitle       = entityArticleTitle entity
    , articleSlug        = entityArticleSlug entity
    , articleDescription = entityArticleDescription entity
    , articleBody        = entityArticleBody entity
    , articleTags        = []
    , articleCreatedAt   = entityArticleCreatedAt entity
    , articleUpdatedAt   = entityArticleUpdatedAt entity
    }

insertArticleStmt :: Article -> Insert [ArticleId]
insertArticleStmt article = Insert
    { into = articleSchema
    , rows = [ ArticleEntity
                { entityArticleId = unsafeCastExpr $ nextval "articles_article_id_seq"
                , entityArticleAuthorId = lit (articleAuthorId article)
                , entityArticleTitle = lit (articleTitle article)
                , entityArticleSlug = lit (articleSlug article)
                , entityArticleDescription = lit (articleDescription article)
                , entityArticleBody  = lit (articleBody article)
                , entityArticleCreatedAt = lit (articleCreatedAt article)
                , entityArticleUpdatedAt = lit (articleUpdatedAt article)
                }
            ]
    , onConflict = Abort
    , returning = Projection entityArticleId
    }

getArticleEntityByIdStmt :: ArticleId -> Query (ArticleEntity Expr)
getArticleEntityByIdStmt id = do
    article <- each articleSchema
    where_ $ entityArticleId article ==. lit id
    return article

getArticleEntityBySlugStmt :: Slug -> Query (ArticleEntity Expr)
getArticleEntityBySlugStmt slug = do
    article <- each articleSchema
    where_ $ entityArticleSlug article ==. lit slug
    return article
