module Hockey.Processing (
    module DB,
    getDates,
    getGames,
    getVideos,
    getEvents
)

where

import Hockey.Requests
import Hockey.Types as T
import Hockey.Database as DB
import Hockey.Formatting
import Data.List as List

fetchDates :: (Integer, Integer) -> IO [T.GameDate]
fetchDates date = do
    results <- (getGameDates date)
    case results of
        Just value -> return (dates value)
        Nothing -> return []

getDates :: [(Integer, Integer)] -> IO [T.GameDate]
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

fetchVideos :: DB.Game -> DB.Video
fetchVideos game = dbVideo game

getVideos :: [DB.Game] -> IO [DB.Video]
getVideos xs = return $ List.map fetchVideos xs

pickTeam :: Int -> (Int, String) -> (Int, String) -> String
pickTeam teamId (awayId, awayName) (homeId, homeName)
    | teamId == awayId = teamIdFromName awayName
    | teamId == homeId = teamIdFromName homeName
    | otherwise = ""

dbEvent :: Int -> T.Event -> (Int, String) -> (Int, String) -> DB.Event
dbEvent gameId event awayTeam homeTeam = DB.Event (eventId event) gameId (pickTeam (teamId event) awayTeam homeTeam) (period event) (time event) (eventType event) (description event) "" (formalId event) (strength event)

convertEvents :: Int -> [T.Event] -> (Int, String) -> (Int, String) -> [DB.Event]
convertEvents _ [] _ _ = []
convertEvents gameId (x:xs) awayTeam homeTeam = [(dbEvent gameId x awayTeam homeTeam)] ++ (convertEvents gameId xs awayTeam homeTeam)

convertGameEvents :: GameEvents -> EventGame
convertGameEvents e = game (eventData e)

fetchEvents :: DB.Game -> IO [DB.Event]
fetchEvents game = do
    results <- let gameId = (gameGameId game)
               in getGameEvents (yearFromGameId gameId) (seasonFromGameId gameId) (gameFromGameId gameId)
    case results of
        Just value -> let e = (convertGameEvents value)
                      in return $ convertEvents (gameGameId game) (play (plays e)) ((awayTeamId e), (awayName e)) ((homeTeamId e), (homeName e))
        Nothing -> return []

getEvents :: [DB.Game] -> IO [DB.Event]
getEvents [] = return []
getEvents (x:xs) = do
    h <- fetchEvents x
    t <- getEvents xs
    return $ h ++ t
