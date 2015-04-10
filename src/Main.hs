import Hockey.Database
import Hockey.Processing
import Hockey.Formatting
import Data.List as List

currentYear :: (Integer, Integer)
currentYear = (2013, 2014)

currentSeason :: Season
currentSeason = Playoffs

preseasonMonths :: (Integer, Integer) -> [(Integer, Integer)]
preseasonMonths year = [((fst year), 9), ((fst year), 10)]

seasonMonths :: (Integer, Integer) -> [(Integer, Integer)]
seasonMonths year = [((fst year), 10), ((fst year), 11), ((fst year), 12), ((snd year), 1), ((snd year), 2), ((snd year), 3), ((snd year), 4)]

playoffMonths :: (Integer, Integer) -> [(Integer, Integer)]
playoffMonths year = [((snd year), 4), ((snd year), 5), ((snd year), 6)]

months :: Season -> (Integer, Integer) -> [(Integer, Integer)]
months Preseason years= preseasonMonths years
months Season years = seasonMonths years
months Playoffs years = playoffMonths years

database :: Database
database = (Database Postgres "pierremarcairoldi" "localhost" "pierremarcairoldi" "" 5432 10)
-- database = (Database SQLite "" "" "" "" 0 10)

run :: Season -> (Integer, Integer) -> IO ()
run s y = do
    migrate database

    dates <- getDates (months s y)

    games <- getGames $ List.map date (filter (\x -> (season x) == s) dates)
    database `process` (upsertMany games)

    videos <- getVideos games
    database `process` (upsertMany videos)

cmpSeason :: GameDate -> Bool
cmpSeason s = (season s) == currentSeason

main :: IO ()
main = do
    -- run currentSeason currentYear

    migrate database

    -- dates <- getDates (months s y)

    games <- getGames $ [dateFromComponents 2015 04 09]
    database `process` (upsertMany games)

    -- videos <- getVideos games
    -- database `process` (upsertMany videos)
    --
    -- events <- getEvents games
    -- database `process` (upsertMany events)
