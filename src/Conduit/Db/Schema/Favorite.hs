{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}

module Conduit.Db.Schema.Favorite where

import Conduit.App
import Conduit.Core.Article
import Conduit.Core.User
import RIO
import Rel8

data FavoriteEntity f = FavoriteEntity
    { favoriteUserId :: Column f UserId
    , favoriteArticleId :: Column f ArticleId
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (FavoriteEntity f)

favoriteSchema :: TableSchema (FavoriteEntity Name)
favoriteSchema =
    TableSchema
        { name = "favorited"
        , schema = Nothing
        , columns =
            FavoriteEntity
                { favoriteUserId = "favor_user_id"
                , favoriteArticleId = "favor_article_id"
                }
        }

addFavoritedArticleStmt :: UserId -> ArticleId -> Insert Int64
addFavoritedArticleStmt currentUserId targetArticleId =
    Insert
        { into = favoriteSchema
        , rows = values [FavoriteEntity (lit currentUserId) (lit targetArticleId)]
        , onConflict = DoNothing
        , returning = NumberOfRowsAffected
        }

removeFavoritedArticleStmt :: UserId -> ArticleId -> Delete Int64
removeFavoritedArticleStmt userId articleId' =
    Delete
        { from = favoriteSchema
        , using = pure ()
        , deleteWhere = \_ row -> favoriteArticleId row ==. lit articleId' &&. favoriteUserId row ==. lit userId
        , returning = NumberOfRowsAffected
        }

removeAllFavoritesByArticleIdStmt :: ArticleId -> Delete Int64
removeAllFavoritesByArticleIdStmt articleId' =
    Delete
        { from = favoriteSchema
        , using = pure ()
        , deleteWhere = \_ row -> favoriteArticleId row ==. lit articleId'
        , returning = NumberOfRowsAffected
        }

getFavoritedCountOfArticleStmt :: Expr ArticleId -> Query (Expr Int64)
getFavoritedCountOfArticleStmt articleId = countRows $ do
    favorite <- each favoriteSchema
    where_ $ articleId ==. favoriteArticleId favorite

checkFavoriteStmt :: UserId -> Expr ArticleId -> Query (Expr Bool)
checkFavoriteStmt userId articleId = exists $ do
    favorite <- each favoriteSchema
    where_ $ (favoriteUserId favorite ==. lit userId) &&. (favoriteArticleId favorite ==. articleId)
