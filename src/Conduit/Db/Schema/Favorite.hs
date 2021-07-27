{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Conduit.Db.Schema.Favorite where

import RIO
import Rel8

import Conduit.App
import Conduit.Core.User
import Conduit.Core.Article

data FavoriteEntity f = FavoriteEntity
    { favoriteUserId    :: Column f UserId
    , favoriteArticleId :: Column f ArticleId
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (FavoriteEntity f)

favoriteSchema :: TableSchema (FavoriteEntity Name)
favoriteSchema = TableSchema
    { name = "favorited"
    , schema = Nothing
    , columns = FavoriteEntity
        { favoriteUserId = "favor_user_id"
        , favoriteArticleId = "favor_article_id"
        }
    }

checkFavoriteStmt :: UserId -> Expr ArticleId -> Query (Expr Bool)
checkFavoriteStmt userId articleId = exists $ do
    favorite <- each favoriteSchema
    where_ $ (favoriteUserId favorite ==. lit userId) &&. (favoriteArticleId favorite ==. articleId)
