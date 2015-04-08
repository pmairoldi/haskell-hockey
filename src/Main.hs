import Hockey.Database
import Hockey.Processing
import Hockey.Formatting
import Data.List as List

database :: Database
database = (Database Postgres "pierremarcairoldi" "localhost" "pierremarcairoldi" "" 5432 10)
-- database = (Database SQLite "" "" "" "" 0 10)

begin :: Day
begin = (dateFromComponents 2014 4 6)

end :: Day
end = (dateFromComponents 2014 4 9)

run :: [Day] -> IO ()
run days = do
    migrate database

    dates <- getDates days

    games <- getGames $ List.map date dates
    database `process` (upsertMany games)

    videos <- getVideos games
    database `process` (upsertMany videos)

currentSeason :: Season
currentSeason = Playoffs

cmpSeason :: GameDate -> Bool
cmpSeason s = (season s) == Playoffs

main :: IO ()
main = do
    -- run [begin..end]
    -- this return too many dates bcause of recursion in the get dates
    dates <- getDates [begin..end]

    print $ List.map date (filter cmpSeason dates)
