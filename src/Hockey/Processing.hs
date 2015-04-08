module Hockey.Processing (
    processGames
)

where

import Hockey.Requests
import Hockey.Types as T
import Hockey.Database as DB
import Hockey.Formatting
import Data.Time.Calendar
-- import Data.Time.LocalTime

gameToDB :: T.Game -> Day -> DB.Game
gameToDB game date = DB.Game (gameId game) (awayId game) (homeId game) date (T.gameTime game) (joinStrings (caTV game) (usTV game)) (T.gameState game) (periodFromPeriodTime $ periodTime game) (periodTime game) (awayScore game) (homeScore game) (awaySog game) (homeSog game) "" ""

convertGames :: [T.Game] -> Day -> [DB.Game]
convertGames [] _ = []
convertGames (x:xs) d = [(gameToDB x d)] ++ (convertGames xs d)

fetchResults :: Day -> IO [DB.Game]
fetchResults date = do
    results <- (getResults date)
    case results of
        Just value -> return (convertGames (games value) (currentDate value))
        Nothing -> return []

insertGames :: Database -> Day -> IO ()
insertGames database date = do
    results <- fetchResults date
    connect database $ upsertMany results

processGames :: Database -> Day -> Day -> IO ()
processGames database begin end
    | begin == end = insertGames database begin
    | begin < end = do
        insertGames database begin
        processGames database (addDays 1 begin) end
    | begin > end = do
        insertGames database end
        processGames database begin (addDays 1 end)
    | otherwise = return ()
