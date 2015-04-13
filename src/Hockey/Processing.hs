module Hockey.Processing (
    module DB,
    getDates,
    processTeams,
    processGames,
    processEvents,
    processVideos,
    processSeeds,
    processPeriods
)

where

import Hockey.Requests
import Hockey.Types as T
import Hockey.Database as DB
import Hockey.Formatting
import Data.List as List
import Hockey.Playoffs as P

dbTeam :: T.Team -> DB.Team
dbTeam team = DB.Team (T.abr team) (T.city team) (T.name team)

getTeams :: [T.Team] -> [DB.Team]
getTeams teams = List.map dbTeam teams

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
dbGame game date = DB.Game (gameId game) (T.awayId game) (T.homeId game) date (T.gameTime game) (joinStrings (caTV game) (usTV game)) (T.gameState game) (periodFromPeriodTime $ periodTime game) (periodTime game) (awayScore game) (homeScore game) (awaySog game) (homeSog game) "" ""

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

-- fix getting videos
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

dbSeed :: Seed -> DB.PlayoffSeed
dbSeed seed = DB.PlayoffSeed (P.year seed) (P.conference seed) (P.round seed) (P.seed seed) (P.homeId seed) (P.awayId seed)

getSeeds :: [Seed] -> [DB.PlayoffSeed]
getSeeds seeds = List.map dbSeed seeds

dbPeriod :: Int -> String -> Int -> PeriodData -> DB.Period
dbPeriod gameId team period periodData = DB.Period gameId team period (shots periodData) (goals periodData)

convertPeriods :: Int -> String -> Int -> [PeriodData] -> [DB.Period]
convertPeriods gameId team period [] = []
convertPeriods gameId team period (x:xs) = [(dbPeriod gameId team period x)] ++ (convertPeriods gameId team (period + 1) xs)

fetchPeriods :: DB.Game -> IO [DB.Period]
fetchPeriods game = do
    results <- let gameId = (gameGameId game)
               in getGamePeriods (yearFromGameId gameId) (seasonFromGameId gameId) (gameFromGameId gameId)
    case results of
        Just value -> let h = (home value)
                          a = (away value)
                      in return $ (convertPeriods (gameGameId game) (T.periodTeamId h) 1 (periods h)) ++ (convertPeriods (gameGameId game) (T.periodTeamId a) 1 (periods a))
        Nothing -> return []

getPeriods :: [DB.Game] -> IO [DB.Period]
getPeriods [] = return []
getPeriods (x:xs) = do
    h <- fetchPeriods x
    t <- getPeriods xs
    return $ h ++ t

-- exposed functions
processTeams :: Database -> [T.Team] -> IO ()
processTeams db teams = db `process` (upsertMany (getTeams teams))

processSeeds :: Database -> [Seed] -> IO ()
processSeeds db seeds = db `process` (insertManyUnique (getSeeds seeds))

processGames :: Database -> [Day] -> IO ()
processGames db xs = do
    values <- getGames xs
    db `process` (upsertMany values)

processPeriods :: Database -> [DB.Game] -> IO ()
processPeriods db xs = do
    values <- getPeriods xs
    db `process` (upsertMany values)

processEvents :: Database -> [DB.Game] -> IO ()
processEvents db xs = do
    values <- getEvents xs
    db `process` (upsertMany values)

processVideos :: Database -> [DB.Game] -> IO ()
processVideos db xs = do
    values <- getVideos xs
    db `process` (upsertMany values)
