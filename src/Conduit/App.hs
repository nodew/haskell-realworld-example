{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Conduit.App where

import RIO
import Hasql.Pool (Pool)
import Crypto.JOSE.JWK ( JWK )
import Control.Monad.Trans.Except ( ExceptT(ExceptT) )
import qualified Servant

data AppEnv = AppEnv
    { envDbPool :: Pool
    , envJwtKey :: JWK
    }

type AppM = RIO AppEnv

runHandler ∷ AppEnv → AppM a → Servant.Handler a
runHandler env app = Servant.Handler $ ExceptT $ try $ runRIO env app

getDbPool :: AppM Pool
getDbPool = ask <&> envDbPool

getJwtKey :: AppM JWK
getJwtKey = ask <&> envJwtKey
