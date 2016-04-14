module Hockey.Processing (
    module DB,
    getDates,
    processTeams,
    processGames,
    processEvents,
    processSeeds,
    processPeriods,
    processSeries,
    getSeries,
    filterSeries
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

combineGameDates :: [T.GameDates] -> [T.Game]
combineGameDates [] = []
combineGameDates (x:xs) = (games x) ++ (combineGameDates xs)

mergePeriods :: Int -> String -> String -> [T.Period] -> [DB.Period]
mergePeriods gameId homeAbr awayAbr [] = []
mergePeriods gameId homeAbr awayAbr (x:xs) = do
    let period = (periodId x)
    let homeStats = (homePeriod x)
    let awayStats = (awayPeriod x)
    let homePeriod = dbPeriod gameId homeAbr period homeStats
    let awayPeriod = dbPeriod gameId awayAbr period awayStats
    let periods = [homePeriod, awayPeriod]

    periods ++ (mergePeriods gameId homeAbr awayAbr xs)

combinePeriods :: [T.Game] -> [DB.Period]
combinePeriods [] = []
combinePeriods (x:xs) = do
    let gameId = (T.gameId x)
    let teams = (T.teams x)
    let home = (teamAbr (team (homeInfo teams)))
    let away = (teamAbr (team (awayInfo teams)))
    let linescore = (T.linescore x)
    let periods = (gamePeriods linescore)

    (mergePeriods gameId home away periods) ++ (combinePeriods xs)

fetchDates :: (Integer, Integer) -> IO [T.Game]
fetchDates date = do
    results <- (getGameDates date)
    case results of
        Just value -> return $ combineGameDates (dates value)
        Nothing -> return []

getDates :: [(Integer, Integer)] -> IO [T.Game]
getDates [] = return []
getDates (x:xs) = do
        h <- fetchDates x
        t <- getDates xs
        return $ h ++ t

compareTimes :: TimeOfDay -> TimeOfDay -> TimeOfDay
compareTimes currentTime fetchedTime
    | currentTime == fetchedTime = fetchedTime
    | fetchedTime == (timeFromComponents 0 0) = currentTime
    | otherwise = fetchedTime

getTeamId :: TeamInfo -> String
getTeamId x = T.teamAbr $ T.team x

getTeamScore :: ScoreInfo -> Int
getTeamScore x = T.goals x

getTeamShots :: ScoreInfo -> Int
getTeamShots x = T.shots x

getBroadcastList :: [Broadcast] -> String
getBroadcastList [] = ""
getBroadcastList [x] = (broadcastName x)
getBroadcastList (x:xs) = (broadcastName x) ++ "," ++ (getBroadcastList xs)

tbdOrTime :: GameState -> String -> String
tbdOrTime status time
    | status == TBD = "00:00:00"
    | otherwise = time

dbGame :: T.Game -> Day -> TimeOfDay -> (String, String) -> DB.Game
dbGame game date time videos = DB.Game (yearFromGameId (gameId game)) (seasonFromGameId (gameId game)) (gameId game) (getTeamId (T.awayInfo (T.teams game))) (getTeamId (T.homeInfo (T.teams game))) date time (getBroadcastList (T.broadcasts game)) (T.gameState (T.status game)) (T.gamePeriod (T.linescore game)) (tbdOrTime (T.gameState (T.status game)) (T.periodTime (T.linescore game))) (getTeamScore (T.awayTeam (T.scoreTeams (T.linescore game)))) (getTeamScore (T.homeTeam (T.scoreTeams (T.linescore game)))) (getTeamShots (T.awayTeam (T.scoreTeams (T.linescore game)))) (getTeamShots (T.homeTeam (T.scoreTeams (T.linescore game)))) "" "" (fst videos) (snd videos) [] [] True

convertGames :: Database -> [T.Game] -> IO [DB.Game]
convertGames _ [] = return $ []
convertGames db (x:xs) = do
    time <- selectTimeForGame db (gameId x)
    videos <- fetchVideos x (gameDay (date x))
    convGames <- (convertGames db xs)

    return $ convGames ++ [(dbGame x (gameDay (date x)) (compareTimes time $ T.gameTime (date x)) videos)]

fetchGames :: Database -> Day -> Day -> IO ([DB.Game], [DB.Period])
fetchGames db from to = do
    results <- (getResults from to)
    case results of
        Just value -> do
            let games = (combineGameDates (dates value))
            dbGames <- (convertGames db games)
            let dbPeriods = (combinePeriods games)
            return (dbGames, dbPeriods)
        Nothing -> return ([], [])

getGames :: Database -> Day -> Day -> IO ([DB.Game], [DB.Period])
getGames db from to = fetchGames db from to

processLink :: Maybe String -> String
processLink link = case link of
    Just value -> value
    Nothing -> []

processYear :: T.Game -> Year
processYear game = (intToInteger (yearFromGameId (gameId game)), intToInteger (yearFromGameId (gameId game)) + 1)

fetchVideos :: T.Game -> Day -> IO (String, String)
fetchVideos game date = do
    homeHighlight <- getVideo date (processYear game) (seasonFromGameId (gameId game)) (gameId game) (getTeamId (T.awayInfo (T.teams game))) (getTeamId (T.homeInfo (T.teams game))) Home
    awayHighlight <- getVideo date (processYear game) (seasonFromGameId (gameId game)) (gameId game) (getTeamId (T.awayInfo (T.teams game))) (getTeamId (T.homeInfo (T.teams game))) Away

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
dbSeed seed = DB.PlayoffSeed (P.year seed) (P.conference seed) (P.round seed) (P.seed seed) (P.homeId seed) (P.awayId seed) 0 0

getSeeds :: [Seed] -> [DB.PlayoffSeed]
getSeeds seeds = List.map dbSeed seeds

dbPeriod :: Int -> String -> Int -> T.PeriodData -> DB.Period
dbPeriod gameId team period periodData = DB.Period (yearFromGameId gameId) (seasonFromGameId gameId) gameId team period (T.periodShots periodData) (T.periodGoals periodData)

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

getSeries :: Database -> Year -> [DB.PlayoffSeed] -> IO [(DB.PlayoffSeed, [DB.Game])]
getSeries db year [] = return []
getSeries db year (x:xs) = do
    h <- selectGamesForSeries db year (playoffSeedHomeId x) (playoffSeedAwayId x)
    t <- getSeries db year xs
    case h of
        [] -> return $ t
        otherwise -> return $ [(x, h)] ++ t

compareTeamScore :: DB.Game -> String -> Bool
compareTeamScore game team
    | (DB.gameState game) /= Final = False
    | team == (gameHomeId game) = homeScore > awayScore
    | team == (gameAwayId game) = awayScore > homeScore
    | otherwise = False
    where
        homeScore = (gameHomeScore game)
        awayScore = (gameAwayScore game)

completedSeries :: (DB.PlayoffSeed, [DB.Game]) -> Maybe (String, (DB.PlayoffSeed, [DB.Game]))
completedSeries series
    | topTeamWins == 4 = Just (topTeam, series)
    | bottomTeamWins == 4 = Just (bottomTeam, series)
    | otherwise = Nothing
    where
        topTeamWins = List.foldl (\acc x -> if (compareTeamScore x (playoffSeedHomeId (fst series))) then acc + 1 else acc) 0 (snd series)
        bottomTeamWins = List.foldl (\acc x -> if (compareTeamScore x (playoffSeedAwayId (fst series))) then acc + 1 else acc) 0 (snd series)
        topTeam = (playoffSeedHomeId (fst series))
        bottomTeam = (playoffSeedAwayId (fst series))

filterSeries :: [(DB.PlayoffSeed, [DB.Game])] -> [(String, (DB.PlayoffSeed, [DB.Game]))]
filterSeries [] = []
filterSeries (x:xs) =
    case (completedSeries x) of
        Just (value) -> [value] ++ filterSeries xs
        Nothing -> filterSeries xs

-- exposed functions
processTeams :: Database -> [T.Team] -> IO ()
processTeams db teams = db `process` (upsertMany (getTeams teams))

processSeeds :: Database -> [Seed] -> IO ()
processSeeds db seeds = db `process` (insertManyUnique (getSeeds seeds))

processGames :: Database -> Day -> Day -> IO ()
processGames db from to = do
    values <- getGames db from to
    db `process` (upsertMany (fst values))
    db `process` (upsertMany (snd values))

processPeriods :: Database -> [DB.Game] -> IO ()
processPeriods db xs = do
    values <- getPeriods xs
    db `process` (upsertMany values)

processEvents :: Database -> [DB.Game] -> IO ()
processEvents db xs = do
    values <- getEvents xs
    db `process` (upsertMany values)

processSeries :: Database -> Year -> IO ()
processSeries db year = do
    seeds <- selectSeeds db year
    series <- getSeries db year seeds

    let filteredSeries = filterSeries series
    let updatedSeeds = (updateSeeds year $ List.map (\x -> (fst x, fst (snd x))) filteredSeries)

    db `process` (upsertMany updatedSeeds)
    updateGamesToInactive db $ List.concat $ List.map (\x -> snd x) $ List.map snd filteredSeries
