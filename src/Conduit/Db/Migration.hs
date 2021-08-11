{-# LANGUAGE TemplateHaskell #-}
module Conduit.Db.Migration where

import RIO
import Hasql.Migration
import Hasql.Migration.Util
import Hasql.Transaction
import Hasql.Pool
import Data.FileEmbed

import Conduit.Db.Transaction

scripts :: [(FilePath, ByteString)]
scripts = $(embedDir "sql")

migrationCommands :: [MigrationCommand]
migrationCommands = map (uncurry MigrationScript) scripts

autoMigrate :: MonadIO m => Pool -> m (Maybe MigrationError)
autoMigrate pool = do
    runTransactionWithPool pool initializeMigrationSchema
    runTransactionWithPool pool $ runMigrations migrationCommands

runMigrations :: [MigrationCommand] -> Transaction (Maybe MigrationError)
runMigrations []  = pure Nothing
runMigrations (x:xs) = do
    err <- runMigration x
    case err of
        Nothing -> runMigrations xs
        Just _  -> return err

initializeMigrationSchema :: Transaction ()
initializeMigrationSchema = do
    exist <- existsTable "schema_migrations"
    unless exist $ do
        sql $ mconcat
            [ "create table if not exists schema_migrations "
            , "( filename varchar(512) not null"
            , ", checksum varchar(32) not null"
            , ", executed_at timestamp without time zone not null default now() "
            , ");"
            ]
