{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- Test Modules
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Database.Persist
import Database.Persist.Postgresql hiding (Connection)
import Database.Persist.Sqlite hiding (Connection)
import Database.Persist.TH
import Data.Text
-- My Modules
import Hockey.Requests
import Hockey.Types
import Hockey.Formatting
import Hockey.Parsing

-- Load from environment variables
currentYear :: Integer
currentYear = 2014

currentSeason :: Season
currentSeason = Playoffs

-- main :: IO()
-- main = do
--     results <- getResults $ dateFromComponents 2015 3 29
--     print results

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

data Database = Database {
    host :: String,
    name :: String,
    user :: String,
    password :: String,
    port :: Integer
} deriving (Show)


connectionNumber :: Int
connectionNumber = 10

postgresConnection :: ConnectionString
postgresConnection = "host=localhost dbname=pierremarcairoldi user=pierremarcairoldi password= port=5432"

sqliteConnection :: Text
sqliteConnection = ":memory:"

type Query m a = ConnectionPool -> m a
type Driver m a = Query m a -> m a
type Logger a b = a -> b
type Connection m a b = Query m a -> b
type Result a b = SqlPersistM a -> b

query :: (MonadIO m) => SqlPersistM a -> Query m a
query stmt = \pool -> liftIO $ runSqlPersistMPool stmt pool

-- have way to sway between types
postgres :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Driver m a
postgres query = withPostgresqlPool postgresConnection connectionNumber query

sqlite :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Driver m a
sqlite query = withSqlitePool sqliteConnection connectionNumber query

db :: MonadIO m => Logger b c -> Connection m a b -> Result a c
db logger connection stmt = logger $ connection $ query stmt

testDB =  do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print (john :: Maybe Person)

    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]

main :: IO ()
main = do
    db runStderrLoggingT postgres testDB
    db runStderrLoggingT sqlite testDB
