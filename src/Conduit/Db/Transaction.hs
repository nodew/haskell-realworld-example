module Conduit.Db.Transaction where

import Conduit.App
import Conduit.Config
import Conduit.Environment
import Data.List
import Data.Time.Clock (secondsToDiffTime, DiffTime)
import Hasql.Connection (Connection)
import Hasql.Pool (Pool, UsageError (..), acquire)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, condemn, sql, statement)
import qualified Hasql.Transaction.Sessions as Hasql
import RIO

connectionAcquisitionTimeout :: DiffTime
connectionAcquisitionTimeout = secondsToDiffTime 10 -- 10 seconds

maximalConnectionLifetime :: DiffTime
maximalConnectionLifetime = secondsToDiffTime 1800 -- 30 minutes

loadPool :: ByteString -> Int -> IO Pool
loadPool connectString poolSize = acquire poolSize connectionAcquisitionTimeout maximalConnectionLifetime connectString

runTransactionWithConnection :: MonadIO m => Connection -> Transaction b -> m b
runTransactionWithConnection conn transaction = do
    e <- liftIO $ Session.run (Hasql.transaction Hasql.Serializable Hasql.Write transaction) conn
    either throwIO pure e

runTransactionWithPool :: MonadIO m => Pool -> Transaction b -> m b
runTransactionWithPool pool transaction = do
    result <- liftIO $ Pool.use pool (Hasql.transaction Hasql.Serializable Hasql.Write transaction)
    case result of
        Right e -> pure e
        Left (ConnectionUsageError e) -> error $ "Failed to connect to database, error: " ++ show e
        Left (SessionUsageError e) -> throwIO e
        Left AcquisitionTimeoutUsageError -> error "Timeout"

runStmt :: Statement () a -> Transaction a
runStmt = statement ()

runTransaction :: forall a m env. (HasDbPool env, MonadReader env m, MonadIO m) => Transaction a -> m a
runTransaction transaction = do
    pool <- getDbPool'
    runTransactionWithPool pool transaction

executeStmt :: Statement () a -> AppM a
executeStmt = runTransaction . runStmt

truncateTables :: [Text] -> Transaction ()
truncateTables tables =
    sql $
        mconcat
            [ "TRUNCATE "
            , fromString $ intercalate ", " (map show tables)
            , " RESTART IDENTITY;"
            ]
