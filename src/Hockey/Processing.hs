module Hockey.Processing (
    module DB,
    getDates,
    getGames,
    getVideos
)

where

import Hockey.Requests
import Hockey.Types as T
import Hockey.Database as DB
import Hockey.Formatting
import Data.List as List

fetchDates :: Day -> IO [T.GameDate]
fetchDates date = do
    results <- (getGameDates date)
    case results of
        Just value -> return (dates value)
        Nothing -> return []

getDates :: [Day] -> IO [T.GameDate]
getDates [] = return []
getDates (x:xs) = do
        h <- fetchDates x
        t <- getDates xs
        return $ h ++ t

dbGame :: T.Game -> Day -> DB.Game
dbGame game date = DB.Game (gameId game) (awayId game) (homeId game) date (T.gameTime game) (joinStrings (caTV game) (usTV game)) (T.gameState game) (periodFromPeriodTime $ periodTime game) (periodTime game) (awayScore game) (homeScore game) (awaySog game) (homeSog game) "" ""

convertGames :: [T.Game] -> Day -> [DB.Game]
convertGames [] _ = []
convertGames (x:xs) d = [(dbGame x d)] ++ (convertGames xs d)

fetchGames :: Day -> IO [DB.Game]
fetchGames date = do
    results <- (getResults date)
    case results of
        Just value -> return (convertGames (games value) (currentDate value))
        Nothing -> return []

getGames :: [Day] -> IO [DB.Game]
getGames [] = return []
getGames (x:xs) = do
        h <- fetchGames x
        t <- getGames xs
        return $ h ++ t

dbVideo :: DB.Game -> DB.Video
dbVideo game = DB.Video (gameGameId game) (gameAwayId game) (gameHomeId game) "" "" "" ""

fetchVideo :: DB.Game -> DB.Video
fetchVideo game = dbVideo game

getVideos :: [DB.Game] -> IO [DB.Video]
getVideos xs = return $ List.map fetchVideo xs
