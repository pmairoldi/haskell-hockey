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

runGameInsert database date = do
    results <- fetchResults date
    run database $ insertGames results

processRange database begin end
    | begin == end = runGameInsert database begin
    | begin < end = do
        runGameInsert database begin
        processRange database (addDays 1 begin) end
    | begin > end = do
        runGameInsert database end
        processRange database begin (addDays 1 end)
    | otherwise = return ()

main :: IO ()
main = do
    migrate postgres

    processRange postgres (dateFromComponents 2014 4 6) (dateFromComponents 2014 4 4)

    return ()
