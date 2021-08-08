{-# LANGUAGE RankNTypes #-}
module Conduit.Db.Helper where

import RIO
import Hasql.Transaction ( Transaction, condemn, statement )
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as Hasql
import Hasql.Statement (Statement)

import Conduit.App

runTransaction :: forall a . Transaction a -> AppM a
runTransaction transaction' = do
    connection <- getConn
    e <- liftIO $ Session.run (Hasql.transaction Hasql.Serializable Hasql.Write transaction') connection
    either throwIO pure e

runStmt :: Statement () a -> Transaction a
runStmt = statement ()

runSimpleStmt :: Statement () a -> AppM a
runSimpleStmt = runTransaction . runStmt
