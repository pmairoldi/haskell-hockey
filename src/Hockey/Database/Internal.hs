{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hockey.Database.Internal (
    Database(..),
    Query,
    Driver,
    Logger,
    Connection,
    Result,
    module Database.Persist,
    run,
    postgres,
    sqlite
)

where

import Database.Persist
import Database.Persist.Postgresql hiding (Connection)
import Database.Persist.Sqlite hiding (Connection)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Text

type Query m a = ConnectionPool -> m a
type Driver m a = Query m a -> m a
type Logger a b = a -> b
type Connection m a b = Query m a -> b
type Result a b = SqlPersistM a -> b

data Database = Database {
    host :: String,
    name :: String,
    user :: String,
    password :: String,
    port :: Integer
} deriving (Show)

query :: (MonadIO m) => SqlPersistM a -> Query m a
query stmt = \pool -> liftIO $ runSqlPersistMPool stmt pool

db :: MonadIO m => Logger b c -> Connection m a b -> Result a c
db logger connection stmt = logger $ connection $ query stmt

logger :: MonadIO m => LoggingT m a -> m a
logger = runStderrLoggingT

run database queries = db logger database queries

--refactor to user Database
connectionNumber :: Int
connectionNumber = 10

postgresConnection :: ConnectionString
postgresConnection = "host=localhost dbname=pierremarcairoldi user=pierremarcairoldi password= port=5432"

sqliteConnection :: Text
sqliteConnection = ":memory:"

postgres :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Driver m a
postgres query = withPostgresqlPool postgresConnection connectionNumber query

sqlite :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Driver m a
sqlite query = withSqlitePool sqliteConnection connectionNumber query
