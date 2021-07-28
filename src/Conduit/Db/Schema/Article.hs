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
    { _articleId          :: Column f ArticleId
    , _articleAuthorId    :: Column f UserId
    , _articleTitle       :: Column f Text
    , _articleSlug        :: Column f Slug
    , _articleDescription :: Column f Text
    , _articleBody        :: Column f Text
    , _articleCreatedAt   :: Column f UTCTime
    , _articleUpdatedAt   :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (ArticleEntity f)

articleSchema :: TableSchema (ArticleEntity Name)
articleSchema = TableSchema
    { name = "articles"
    , schema = Nothing
    , columns = ArticleEntity
        { _articleId          = "article_id"
        , _articleAuthorId    = "article_user_id"
        , _articleTitle       = "article_title"
        , _articleSlug        = "article_slug"
        , _articleDescription = "article_description"
        , _articleBody        = "article_body"
        , _articleCreatedAt   = "article_createdat"
        , _articleUpdatedAt   = "article_updatedat"
        }
    }

mapArticleEntityToArticle :: ArticleEntity Result -> Article
mapArticleEntityToArticle entity = Article
    { articleId          = _articleId entity
    , articleAuthorId    = _articleAuthorId entity
    , articleTitle       = _articleTitle entity
    , articleSlug        = _articleSlug entity
    , articleDescription = _articleDescription entity
    , articleBody        = _articleBody entity
    , articleTags        = []
    , articleCreatedAt   = _articleCreatedAt entity
    , articleUpdatedAt   = _articleUpdatedAt entity
    }

insertArticleStmt :: Article -> Insert [ArticleId]
insertArticleStmt article = Insert
    { into = articleSchema
    , rows = [ ArticleEntity
                { _articleId = unsafeCastExpr $ nextval "articles_article_id_seq"
                , _articleAuthorId = lit (articleAuthorId article)
                , _articleTitle = lit (articleTitle article)
                , _articleSlug = lit (articleSlug article)
                , _articleDescription = lit (articleDescription article)
                , _articleBody  = lit (articleBody article)
                , _articleCreatedAt = lit (articleCreatedAt article)
                , _articleUpdatedAt = lit (articleUpdatedAt article)
                }
            ]
    , onConflict = Abort
    , returning = Projection _articleId
    }

getArticleEntityByIdStmt :: ArticleId -> Query (ArticleEntity Expr)
getArticleEntityByIdStmt id = do
    article <- each articleSchema
    where_ $ _articleId article ==. lit id
    return article

getArticleEntityBySlugStmt :: Slug -> Query (ArticleEntity Expr)
getArticleEntityBySlugStmt slug = do
    article <- each articleSchema
    where_ $ _articleSlug article ==. lit slug
    return article
