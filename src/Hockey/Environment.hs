module Hockey.Environment (
    Environment(..),
    env,
    database
)

where

import Hockey.Database hiding (dbType)
import Hockey.Formatting
import LoadEnv
import System.Environment
import Data.Maybe

data Environment = Environment {
    dbType :: DatabaseType,
    dbHost :: String,
    dbPort :: Int,
    dbUser :: String,
    dbPassword :: String,
    dbConnections :: Int,
    year :: Year,
    season :: Season,
    logType :: LoggingType
} deriving (Show)

env :: IO (Environment)
env = do
    loadEnv

    e <- getEnvironment
    print e

    t <- lookupEnv "DB_TYPE"
    host <- lookupEnv "DB_HOST"
    port <- lookupEnv "DB_PORT"
    user <- lookupEnv "DB_USER"
    pass <- lookupEnv "DB_PASS"
    conn <- lookupEnv "DB_CONN"
    year <- lookupEnv "YEAR"
    seas <- lookupEnv "SEASON"
    logs <- lookupEnv "LOG_TYPE"

    return $ Environment (read (fromJust t) :: DatabaseType) (fromJust host) (stringToInt (fromJust port)) (fromJust user) (fromJust pass) (stringToInt (fromJust conn)) (seasonYears $ stringToInteger (fromJust year)) (read (fromJust seas) :: Season) (read (fromJust logs) :: LoggingType)

database :: Environment -> Database
database env = Database (dbType env) "hockey" (dbHost env) (dbPort env) (dbUser env) (dbPassword env) (dbConnections env) (logType env)
