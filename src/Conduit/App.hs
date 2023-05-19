{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Conduit.App where

import Conduit.Environment
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Crypto.JOSE.JWK (JWK)
import Hasql.Pool (Pool)
import RIO
import qualified Servant

data AppEnv = AppEnv
    { envDbPool :: Pool
    , envJwtKey :: JWK
    }

type AppM = RIO AppEnv

runHandler :: AppEnv -> AppM a -> Servant.Handler a
runHandler env app = Servant.Handler $ ExceptT $ try $ runRIO env app

instance HasDbPool AppEnv where
    getDbPool = envDbPool

instance HasJwtKey AppEnv where
    getJwtKey = envJwtKey
