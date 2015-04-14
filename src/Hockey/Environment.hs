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
    dbName :: String,
    dbHost :: String,
    dbPort :: Int,
    dbUser :: String,
    dbPassword :: String,
    dbConnections :: Int,
    year :: Year,
    season :: Season,
    logType :: LoggingType,
    port :: Int
} deriving (Show)

env :: IO (Environment)
env = do
    loadEnv

    t <- lookupEnv "DB_TYPE"
    name <- lookupEnv "DB_NAME"
    host <- lookupEnv "DB_HOST"
    dbport <- lookupEnv "DB_PORT"
    user <- lookupEnv "DB_USER"
    pass <- lookupEnv "DB_PASS"
    conn <- lookupEnv "DB_CONN"
    year <- lookupEnv "YEAR"
    seas <- lookupEnv "SEASON"
    logs <- lookupEnv "LOG_TYPE"
    port <- lookupEnv "PORT"

    return $ Environment (read (fromJust t) :: DatabaseType) (fromJust name) (fromJust host) (stringToInt (fromJust dbport)) (fromJust user) (fromJust pass) (stringToInt (fromJust conn)) (seasonYears $ stringToInteger (fromJust year)) (read (fromJust seas) :: Season) (read (fromJust logs) :: LoggingType) (getPort port)

getPort :: Maybe String -> Int
getPort p = case p of
    Just v -> (read v :: Int)
    Nothing -> 3000

database :: Environment -> Database
database env = Database (dbType env) (dbName env) (dbHost env) (dbPort env) (dbUser env) (dbPassword env) (dbConnections env) (logType env)
