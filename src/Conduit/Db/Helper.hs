{-# LANGUAGE RankNTypes #-}
module Conduit.Db.Helper where

import RIO
import Hasql.Transaction ( Transaction, condemn, statement )
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as Hasql
import Hasql.Statement (Statement)

import Conduit.App

withTransaction :: forall a b . ((Transaction a -> AppM a) -> AppM b) -> AppM b
withTransaction f = do
    connection <- getConn
    f $ \m -> do
        e <- liftIO $ Session.run (Hasql.transaction Hasql.Serializable Hasql.Write m) connection
        either throwIO pure e

runStmt :: Statement () b -> Transaction b
runStmt = statement ()
