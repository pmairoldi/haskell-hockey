import Hockey.Database
import Hockey.Processing
import Hockey.Formatting hiding (season, year)
import Data.List as List

import LoadEnv
import System.Environment (lookupEnv)
import Data.Maybe

data Environment = Environment {
    dbHost :: String,
    dbPort :: Int,
    dbUser :: String,
    dbPassword :: String,
    year :: Year,
    season :: Season,
    logType :: LoggingType
} deriving (Show)

env :: IO (Environment)
env = do
    loadEnv

    host <- lookupEnv "DB_HOST"
    port <- lookupEnv "DB_PORT"
    user <- lookupEnv "DB_USER"
    pass <- lookupEnv "DB_PASS"
    year <- lookupEnv "YEAR"
    seas <- lookupEnv "SEASON"
    logs <- lookupEnv "LOG_TYPE"

    return $ Environment (fromJust host) (stringToInt (fromJust port)) (fromJust user) (fromJust pass) (seasonYears $ stringToInteger (fromJust year)) (read (fromJust seas) :: Season) (read (fromJust logs) :: LoggingType)

database :: Environment -> Database
database env = Database Postgres "hockey" (dbHost env) (dbPort env) (dbUser env) (dbPassword env) 10 (logType env)

run :: Database -> Season -> Year -> IO ()
run db s y = do
    migrate db

    dates <- getDates (months s y)

    games <- getGames $ List.map date (filter (\x -> x `cmpSeason` s) dates)
    db `process` (upsertMany games)

    events <- getEvents games
    db `process` (upsertMany events)

    videos <- getVideos games
    db `process` (upsertMany videos)

main :: IO ()
main = do
    e <- env
    run (database e) (season e) (year e)
