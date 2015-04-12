{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hockey.Database.Internal (
    Database(..),
    DatabaseType(..),
    LoggingType(..),
    process
)

where

import Database.Persist
import Database.Persist.Postgresql hiding (Connection)
import Database.Persist.Sqlite hiding (Connection)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Text as T
import Data.ByteString.Char8 as BS

type Query m a = ConnectionPool -> m a
type Driver m a = Query m a -> m a
type Logger a b = a -> b
type Connection m a b = Query m a -> b
type Result a b = SqlPersistM a -> b

data DatabaseType = Postgres | SQLite deriving (Enum, Show, Read, Eq)

data LoggingType = None | Debug deriving (Enum, Show, Read, Eq)

data Database = Database {
    dbType :: DatabaseType,
    name :: String,
    host :: String,
    port :: Int,
    user :: String,
    password :: String,
    connections :: Int,
    logging :: LoggingType
} deriving (Show)

query :: (MonadIO m) => SqlPersistM a -> Query m a
query stmt = \pool -> liftIO $ runSqlPersistMPool stmt pool

db :: MonadIO m => Logger b c -> Connection m a b -> Result a c
db logger connection stmt = logger $ connection $ query stmt

postgresConnection :: Database -> ConnectionString
postgresConnection database = BS.pack $ "host=" ++ (host database) ++ " dbname=" ++ (name database) ++ " user=" ++ (user database) ++  " password=" ++ (password database) ++ " port=" ++ (show $ port database)

sqliteConnection :: Database -> Text
sqliteConnection database
    | (name database) == [] = ":memory:"
    | otherwise = T.pack (name database)

postgres :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Database -> Driver m a
postgres database query = withPostgresqlPool (postgresConnection database) (connections database) query

sqlite :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Database -> Driver m a
sqlite database query = withSqlitePool (sqliteConnection database) (connections database) query

connection :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Database -> Driver m a
connection database = case (dbType database) of
    Postgres -> postgres database
    SQLite -> sqlite database

process :: (MonadBaseControl IO m, MonadIO m) => Database -> Result a (m a)
process database queries = case (logging database) of
    None -> db runNoLoggingT (connection database) queries
    Debug -> db runStderrLoggingT (connection database) queries
