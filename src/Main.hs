import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Maybe
-- My Modules
import Hockey.Requests
import Hockey.Types as T

import Hockey.Formatting
import Hockey.Database as DB

fetchResults :: Day -> IO [DB.Game]
fetchResults date = do
    results <- (getResults date)
    case results of
        Just value -> return (processGames (games value) (currentDate value))
        Nothing -> return []

runGameInsert :: Database -> Day -> IO ()
runGameInsert database date = do
    results <- fetchResults date
    connect database $ insertGames results

processRange :: Database -> Day -> Day -> IO ()
processRange database begin end
    | begin == end = runGameInsert database begin
    | begin < end = do
        runGameInsert database begin
        processRange database (addDays 1 begin) end
    | begin > end = do
        runGameInsert database end
        processRange database begin (addDays 1 end)
    | otherwise = return ()

database :: Database
database = (Database SQLite "test.sqlite" "" "" "" 0 10)

main :: IO ()
main = do
    migrate database

    processRange database (dateFromComponents 2014 4 6) (dateFromComponents 2014 4 4)

    return ()
