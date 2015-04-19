module Hockey.Processing (
    module DB,
    getDates,
    processTeams,
    processGames,
    processEvents,
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

compareTimes :: TimeOfDay -> TimeOfDay -> TimeOfDay
compareTimes currentTime fetchedTime
    | currentTime == fetchedTime = fetchedTime
    | fetchedTime == (timeFromComponents 0 0 AM) = currentTime
    | otherwise = fetchedTime

dbGame :: T.Game -> Day -> TimeOfDay -> (String, String) -> DB.Game
dbGame game date time videos = DB.Game (yearFromGameId (gameId game)) (seasonFromGameId (gameId game)) (gameId game) (T.awayId game) (T.homeId game) date time (joinStrings (caTV game) (usTV game)) (T.gameState game)  (T.gamePeriod game) (periodTime game) (awayScore game) (homeScore game) (awaySog game) (homeSog game) "" "" (fst videos) (snd videos) [] []

convertGames :: Database -> [T.Game] -> Day -> IO [DB.Game]
convertGames _ [] _ = return $ []
convertGames db (x:xs) d = do
    time <- selectTimeForGame db (gameId x)
    videos <- fetchVideos x d
    convGames <- (convertGames db xs d)

    return $ convGames ++ [(dbGame x d (compareTimes time $ T.gameTime x) videos)]

fetchGames :: Database -> Day -> IO [DB.Game]
fetchGames db date = do
    results <- (getResults date)
    case results of
        Just value -> (convertGames db (games value) (currentDate value))
        Nothing -> return []

getGames :: Database -> [Day] -> IO [DB.Game]
getGames _ [] = return []
getGames db (x:xs) = do
        h <- fetchGames db x
        t <- getGames db xs
        return $ h ++ t

processLink :: Maybe String -> String
processLink link = case link of
    Just value -> value
    Nothing -> []

processYear :: T.Game -> Year
processYear game = (intToInteger (yearFromGameId (gameId game)), intToInteger (yearFromGameId (gameId game)) + 1)

fetchVideos :: T.Game -> Day -> IO (String, String)
fetchVideos game date = do
    homeHighlight <- getVideo date (processYear game) (seasonFromGameId (gameId game)) (gameId game) (T.awayId game) (T.homeId game) Home
    awayHighlight <- getVideo date (processYear game) (seasonFromGameId (gameId game)) (gameId game) (T.awayId game) (T.homeId game) Away

    return $ ((processLink awayHighlight), (processLink homeHighlight))

pickTeam :: Int -> (Int, String) -> (Int, String) -> String
pickTeam teamId (awayId, awayName) (homeId, homeName)
    | teamId == awayId = teamIdFromName awayName
    | teamId == homeId = teamIdFromName homeName
    | otherwise = ""

dbEvent :: Int -> T.Event -> (Int, String) -> (Int, String) -> DB.Event
dbEvent gameId event awayTeam homeTeam = DB.Event (eventId event) (yearFromGameId gameId) (seasonFromGameId gameId) gameId (pickTeam (teamId event) awayTeam homeTeam) (period event) (time event) (eventType event) (description event) "" (formalId event) (strength event)

convertEvents :: Int -> [T.Event] -> (Int, String) -> (Int, String) -> [DB.Event]
convertEvents _ [] _ _ = []
convertEvents gameId (x:xs) awayTeam homeTeam = [(dbEvent gameId x awayTeam homeTeam)] ++ (convertEvents gameId xs awayTeam homeTeam)

convertGameEvents :: GameEvents -> EventGame
convertGameEvents e = game (eventData e)

fetchEvents :: DB.Game -> IO [DB.Event]
fetchEvents game = do
    results <- let gameId = (gameGameId game)
               in getGameEvents (intToInteger (yearFromGameId gameId)) (seasonFromGameId gameId) (intToInteger (gameFromGameId gameId))
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
dbSeed seed = DB.PlayoffSeed (P.year seed) Playoffs (P.conference seed) (P.round seed) (P.seed seed) (P.homeId seed) (P.awayId seed)

getSeeds :: [Seed] -> [DB.PlayoffSeed]
getSeeds seeds = List.map dbSeed seeds

dbPeriod :: Int -> String -> Int -> PeriodData -> DB.Period
dbPeriod gameId team period periodData = DB.Period  (yearFromGameId gameId) (seasonFromGameId gameId) gameId team period (shots periodData) (goals periodData)

convertPeriods :: Int -> String -> Int -> [PeriodData] -> [DB.Period]
convertPeriods gameId team period [] = []
convertPeriods gameId team period (x:xs) = [(dbPeriod gameId team period x)] ++ (convertPeriods gameId team (period + 1) xs)

fetchPeriods :: DB.Game -> IO [DB.Period]
fetchPeriods game = do
    results <- let gameId = (gameGameId game)
               in getGamePeriods (intToInteger (yearFromGameId gameId)) (seasonFromGameId gameId) (intToInteger (gameFromGameId gameId))
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

-- insert time only if the state is none
processGames :: Database -> [Day] -> IO ()
processGames db xs = do
    values <- getGames db xs
    db `process` (upsertMany values)

processPeriods :: Database -> [DB.Game] -> IO ()
processPeriods db xs = do
    values <- getPeriods xs
    db `process` (upsertMany values)

processEvents :: Database -> [DB.Game] -> IO ()
processEvents db xs = do
    values <- getEvents xs
    db `process` (upsertMany values)
