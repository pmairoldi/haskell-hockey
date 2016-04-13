import Hockey.Processing
import Hockey.Formatting hiding (dates, season, year)
import Hockey.Environment
import Hockey.Playoffs hiding (year)
import System.Environment (getArgs)
import Data.List as List
import Data.Time hiding (months)

currentDay :: IO Day
currentDay = do
    c <- getCurrentTime
    return $ utctDay c

dates :: Season -> Year -> Day -> Integer -> IO [Day]
dates s y day range = do
    dates <- getDates (months s y) -- split months if this is too long
    return $ filter (\x -> x >= (addDays (-range) day) && x <= (addDays range day)) (List.map date (filter (\x -> (x `cmpSeason` s)) dates))

bootstrap :: Season -> Year -> IO [Day]
bootstrap s y= do
    dates <- getDates (months s y)
    return $ List.map date (filter (\x -> x `cmpSeason` s) dates)

logMsg :: Show a => a -> LoggingType -> IO ()
logMsg msg loggingType = do
    e <- env
    case ((logType e), loggingType) of
        (Debug, Debug) -> print msg
        (Debug, Info) -> print msg
        (Info, Info) -> print msg
        otherwise -> return ()

run :: Database -> Season -> Year -> [Day] -> IO ()
run db s y dates = do

    startTime <- getCurrentTime

    logMsg "Processing Games" Debug
    processGames db dates

    logMsg "Fetch Games" Debug
    games <- selectGames db dates

    logMsg "Processing Periods" Debug
    processPeriods db games

    logMsg "Processing Events" Debug
    processEvents db games

    case s of
        Playoffs -> do
            logMsg "Processing Seeds" Debug
            processSeeds db (seeds y) -- get seeds from standings
            processSeries db y

    endTime <- getCurrentTime

    logMsg (diffUTCTime endTime startTime) Info

    return ()

isBootStrap :: [String] -> Bool
isBootStrap [] = False
isBootStrap (x:xs) = x == "-b"

main :: IO ()
main = do
    e <- env
    args <- getArgs
    day <- currentDay

    let db = (database e)
    let s = (season e)
    let y = (year e)
    let r = intToInteger (range e)

    migrate db

    case isBootStrap args of
        True -> do
            logMsg "Processing Teams" Debug
            processTeams db teamList

            logMsg "Processing Series" Debug
            processSeries db y
            processSeries db y
            processSeries db y
        False -> return ()

    run db s y =<< case isBootStrap args of
        True -> bootstrap s y
        False -> dates s y day r
