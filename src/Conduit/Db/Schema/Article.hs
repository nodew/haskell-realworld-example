{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
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
        , _articleCreatedAt   = "article_createdAt"
        , _articleUpdatedAt   = "article_updatedAt"
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
    , articleCreatedAt   = _articleCreatedAt entity
    , articleUpdatedAt   = _articleUpdatedAt entity        
    }