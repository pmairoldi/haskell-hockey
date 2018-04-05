module Hockey.Processing (
    module DB,
    getDates,
    processTeams,
    processGames,
    processEvents,
    processSeeds,
    processSeries,
    getSeries,
    filterSeries
)

where

import Hockey.Requests
import Hockey.Types as T
import Hockey.Types.Standings as ST
import Hockey.Types.Events as ET
import Hockey.Database as DB
import Hockey.Formatting
import Data.List as List
import Hockey.Playoffs as P
import Data.Maybe

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
    let home = (teamAbr (T.team (homeInfo teams)))
    let away = (teamAbr (T.team (awayInfo teams)))
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

tbdOrTime :: GameState -> TimeOfDay -> TimeOfDay
tbdOrTime status time
    | status == TBD = (timeFromComponents 0 0)
    | otherwise = time

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead [x] = Just x
maybeHead x = Just (List.head x)

getCondense :: [MediaItem] -> Maybe MediaItem
getCondense items = maybeHead (List.filter (\y -> (T.videoType y) == Condense) items)

getQuality :: [VideoLink] -> Maybe VideoLink
getQuality x = maybeHead (List.filter (\y -> (T.quality y) == Flash1800) x)

getUrl :: [VideoLink] -> String
getUrl [] = []
getUrl x = case (getQuality x) of
    Just url -> (T.url url)
    Nothing -> ""

getLinkTypes :: [MediaItem] -> [LinkType]
getLinkTypes media = case (getCondense media) of
    Just item -> T.itemTypes item
    Nothing -> []

getPlaybacks :: [LinkType] -> [VideoLink]
getPlaybacks links = case (maybeHead links) of
    Just x -> T.playbacks x
    Nothing -> []

getVideoUrl :: Maybe T.Media -> String
getVideoUrl (Just x) = getUrl (getPlaybacks (getLinkTypes (T.items x)))
getVideoUrl Nothing = []

dbGame :: T.Game -> Day -> TimeOfDay -> DB.Game
dbGame game date time = DB.Game (yearFromGameId (gameId game)) (seasonFromGameId (gameId game)) (gameId game) (getTeamId (T.awayInfo (T.teams game))) (getTeamId (T.homeInfo (T.teams game))) date (tbdOrTime (T.gameState (T.status game)) time) (getBroadcastList (T.broadcasts game)) (T.gameState (T.status game)) (T.gamePeriod (T.linescore game)) (T.periodTime (T.linescore game)) (getTeamScore (T.awayTeam (T.scoreTeams (T.linescore game)))) (getTeamScore (T.homeTeam (T.scoreTeams (T.linescore game)))) (getTeamShots (T.awayTeam (T.scoreTeams (T.linescore game)))) (getTeamShots (T.homeTeam (T.scoreTeams (T.linescore game)))) "" "" (getVideoUrl (T.media (T.content game))) [] [] [] True

convertGames :: Database -> [T.Game] -> IO [DB.Game]
convertGames _ [] = return $ []
convertGames db (x:xs) = do
    time <- selectTimeForGame db (gameId x)
    convGames <- (convertGames db xs)

    return $ convGames ++ [(dbGame x (gameDay (date x)) (compareTimes time $ T.gameTime (date x)))]

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

dbEvent :: Int -> ET.Play -> DB.Event
dbEvent gameId event = DB.Event (integerToInt (ET.eventId event)) (yearFromGameId gameId) (seasonFromGameId gameId) gameId (ET.playTeam event) (integerToInt (ET.period event)) (ET.time event) (ET.eventType event) (ET.description event) "" (ET.formalId event) (ET.playStrength event)

convertEvents :: Int -> [ET.Play] -> [DB.Event]
convertEvents _ [] = []
convertEvents gameId (x:xs) = dbEvent gameId x : convertEvents gameId xs

fetchEvents :: DB.Game -> IO [DB.Event]
fetchEvents game = do
    results <- let gameId = gameGameId game
               in getGameEvents (intToInteger (yearFromGameId gameId)) (seasonFromGameId gameId) (intToInteger (gameFromGameId gameId))
    case results of
        Just value -> return $ convertEvents (gameGameId game) (ET.allPlays (ET.plays (ET.liveData value)))
        Nothing -> return []

getEvents :: [DB.Game] -> IO [DB.Event]
getEvents [] = return []
getEvents (x:xs) = do
    h <- fetchEvents x
    t <- getEvents xs
    return $ h ++ t

dbSeed :: Seed -> DB.PlayoffSeed
dbSeed seed = DB.PlayoffSeed (P.year seed) (P.conference seed) (P.round seed) (P.seed seed) (P.homeId seed) (P.awayId seed) 0 0

combineStandings :: Standings -> [(StandingsType, StandingTeamRecord)]
combineStandings standings = concatMap (\x -> List.map (\y -> (standingsType x, y)) (teamRecords x)) (records standings)

filterTeamsInPlayoffs :: [(StandingsType, StandingTeamRecord)] -> [(StandingsType, StandingTeamRecord)]
filterTeamsInPlayoffs = filter (\x -> fst x == DivisionLeaders || fst x == WildCard && wildCardRank (snd x) <= 2)

isLeagueLeader :: (StandingsType, StandingTeamRecord) -> Bool
isLeagueLeader x = fst x == DivisionLeaders

isWildCard :: (StandingsType, StandingTeamRecord) -> Bool
isWildCard x = fst x == WildCard

isLeagueLeaderSeed :: (StandingsType, StandingTeamRecord) -> Int -> Bool
isLeagueLeaderSeed x rank = divisionRank (snd x) == rank

isWildCardSeed :: (StandingsType, StandingTeamRecord) -> Int -> Bool
isWildCardSeed x rank = wildCardRank (snd x) == rank

isLeagueLeaderDivision :: (StandingsType, StandingTeamRecord) -> DivisionType -> Bool
isLeagueLeaderDivision x y = division (ST.team (snd x)) == y

isWildCardConference :: (StandingsType, StandingTeamRecord) -> ConferenceType -> Bool
isWildCardConference x y = ST.conference (ST.team (snd x)) == y

findLeagueLeaderTeam :: [(StandingsType, StandingTeamRecord)] -> Int -> DivisionType -> Maybe (StandingsType, StandingTeamRecord)
findLeagueLeaderTeam xs rank division = List.find (\x -> isLeagueLeader x && isLeagueLeaderDivision x division && isLeagueLeaderSeed x rank) xs

getWildCardSeed :: [(StandingsType, StandingTeamRecord)] -> DivisionType -> DivisionType -> Int
getWildCardSeed xs forDivision otherDivision = case (forTeam, otherTeam) of 
    (Just a, Just b) -> if conferenceRank (snd a) < conferenceRank (snd b) then 2 else 1 -- team with best record gets the worst wildcard team
    _ -> 0
    where forTeam = findLeagueLeaderTeam xs 1 forDivision
          otherTeam = findLeagueLeaderTeam xs 1 otherDivision

findWildCardTeam :: [(StandingsType, StandingTeamRecord)] -> Int -> ConferenceType -> Maybe (StandingsType, StandingTeamRecord)
findWildCardTeam xs rank conference = List.find (\x -> isWildCard x && isWildCardConference x conference && isWildCardSeed x rank) xs

getTeamAbr :: Maybe (StandingsType, StandingTeamRecord) -> Maybe String 
getTeamAbr x = case x of 
    Just y -> Just $ abbreviation (ST.team (snd y))
    Nothing -> Nothing

getConference :: Seed -> ConferenceType
getConference seed = case P.conference seed of 
    "e" -> Eastern
    "w" -> Western 

replaceTeams :: Seed -> [(StandingsType, StandingTeamRecord)] -> Seed
replaceTeams seed standings = case (conference, P.seed seed) of 
        (Eastern, 1) -> updateTeams seed (getTeamAbr $ findLeagueLeaderTeam standings 1 Atlantic) (getTeamAbr $ findWildCardTeam standings (getWildCardSeed standings Atlantic Metropolitan) Eastern)
        (Eastern, 2) -> updateTeams seed (getTeamAbr $ findLeagueLeaderTeam standings 2 Atlantic) (getTeamAbr $ findLeagueLeaderTeam standings 3 Atlantic)
        (Eastern, 3) -> updateTeams seed (getTeamAbr $ findLeagueLeaderTeam standings 1 Metropolitan) (getTeamAbr $ findWildCardTeam standings (getWildCardSeed standings Metropolitan Atlantic) Eastern)
        (Eastern, 4) -> updateTeams seed (getTeamAbr $ findLeagueLeaderTeam standings 2 Metropolitan) (getTeamAbr $ findLeagueLeaderTeam standings 3 Metropolitan)
        (Western, 1) -> updateTeams seed (getTeamAbr $ findLeagueLeaderTeam standings 1 Central) (getTeamAbr $ findWildCardTeam standings (getWildCardSeed standings Central Pacific) Western)
        (Western, 2) -> updateTeams seed (getTeamAbr $ findLeagueLeaderTeam standings 2 Central) (getTeamAbr $ findLeagueLeaderTeam standings 3 Central)
        (Western, 3) -> updateTeams seed (getTeamAbr $ findLeagueLeaderTeam standings 1 Pacific) (getTeamAbr $ findWildCardTeam standings (getWildCardSeed standings Pacific Central) Western)
        (Western, 4) -> updateTeams seed (getTeamAbr $ findLeagueLeaderTeam standings 2 Pacific) (getTeamAbr $ findLeagueLeaderTeam standings 3 Pacific)
    where conference = getConference seed
    
convertStandings :: ST.Standings -> [Seed] -> [Seed]
convertStandings standings seeds = round1 ++ otherRounds
    where teams = filterTeamsInPlayoffs (combineStandings standings)
          round1 = List.map (`replaceTeams` teams) (List.filter (\x -> P.round x == 1) seeds)
          otherRounds = List.filter (\x -> P.round x > 1) seeds

fetchStandings :: Year -> IO [DB.PlayoffSeed]
fetchStandings year = do 
    results <- getStandings year
    case results of
        Just value -> return $ List.map dbSeed (convertStandings value (seeds year))
        Nothing -> return $ List.map dbSeed (seeds year)

getSeeds :: Year -> IO [DB.PlayoffSeed]
getSeeds = fetchStandings

dbPeriod :: Int -> String -> Int -> T.PeriodData -> DB.Period
dbPeriod gameId team period periodData = DB.Period (yearFromGameId gameId) (seasonFromGameId gameId) gameId team period (T.periodShots periodData) (T.periodGoals periodData)

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

processSeeds :: Database -> Year -> IO ()
processSeeds db year = do 
    values <- getSeeds year
    db `process` (upsertMany values)    

processGames :: Database -> Day -> Day -> IO ()
processGames db from to = do
    values <- getGames db from to
    db `process` (upsertMany (fst values))
    db `process` (upsertMany (snd values))

filterGoalsAndPenalties :: DB.Event -> Bool
filterGoalsAndPenalties event = eventEventType event == Goal || eventEventType event == Penalty

processEvents :: Database -> [DB.Game] -> IO ()
processEvents db xs = do
    values <- getEvents xs
    db `process` upsertMany (List.filter filterGoalsAndPenalties values)

processSeries :: Database -> Year -> IO ()
processSeries db year = do
    seeds <- selectSeeds db year
    series <- getSeries db year seeds

    let filteredSeries = filterSeries series
    let updatedSeeds = (updateSeeds year $ List.map (\x -> (fst x, fst (snd x))) filteredSeries)

    db `process` (upsertMany updatedSeeds)
    updateGamesToInactive db $ List.concat $ List.map (\x -> snd x) $ List.map snd filteredSeries
