module Conduit.Db.Migration where


import RIO
import Hasql.Migration
import Hasql.Connection
import Hasql.Transaction
import Hasql.Pool

import Conduit.Db.Transaction

migrate :: MonadIO m => Pool -> FilePath -> m (Maybe MigrationError)
migrate pool dir = do
    runTransactionWithPool pool initializeMigrationSchema
    migrations <- liftIO $ loadMigrationsFromDirectory dir
    runTransactionWithPool pool $ runMigrations migrations

runMigrations :: [MigrationCommand] -> Transaction (Maybe MigrationError)
runMigrations []  = pure Nothing
runMigrations (x:xs) = do
    err <- runMigration x
    case err of
        Nothing -> runMigrations xs
        Just _  -> return err

initializeMigrationSchema :: Transaction ()
initializeMigrationSchema =
    sql $ mconcat
        [ "create table if not exists schema_migrations "
        , "( filename varchar(512) not null"
        , ", checksum varchar(32) not null"
        , ", executed_at timestamp without time zone not null default now() "
        , ");"
        ]
