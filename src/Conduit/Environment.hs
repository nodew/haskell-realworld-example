{-# LANGUAGE RankNTypes #-}

module Conduit.Environment where

import Crypto.JOSE (JWK)
import Hasql.Pool (Pool)
import RIO (MonadReader, ask, (<&>))

class HasDbPool env where
    getDbPool :: env -> Pool

class HasJwtKey env where
    getJwtKey :: env -> JWK

getDbPool' :: (HasDbPool env, MonadReader env m) => m Pool
getDbPool' = ask <&> getDbPool

getJwtKey' :: (HasJwtKey env, MonadReader env m) => m JWK
getJwtKey' = ask <&> getJwtKey
