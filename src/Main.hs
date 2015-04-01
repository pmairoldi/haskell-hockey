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
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

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


connStr :: ConnectionString
connStr = "host=localhost dbname=pierremarcairoldi user=pierremarcairoldi password= port=5432"

query :: MonadIO m => SqlPersistM a -> ConnectionPool -> m a
query stmt = \pool -> liftIO $ runSqlPersistMPool stmt pool

db :: SqlPersistM a -> IO a
db stmt = runStderrLoggingT $ withPostgresqlPool connStr 10 $ query stmt

main :: IO ()
main = db $ do
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
