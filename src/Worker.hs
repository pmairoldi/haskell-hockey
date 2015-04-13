import Hockey.Processing
import Hockey.Formatting hiding (dates, season, year)
import Hockey.Environment
import Hockey.Playoffs hiding (year)
import System.Environment (getArgs)
import Data.List as List
import Data.Time

currentDay :: IO Day
currentDay = do
    c <- getCurrentTime
    return $ utctDay c

dates :: Season -> Year -> Day -> IO [Day]
dates s y day = do
    dates <- getDates (months s y) -- split months if this is too long
    return $ filter (\x -> x >= (addDays (-2) day) && x <= (addDays 2 day)) (List.map date (filter (\x -> (x `cmpSeason` s)) dates))

bootstrap :: Season -> Year -> IO [Day]
bootstrap s y= do
    dates <- getDates (months s y)
    return $ List.map date (filter (\x -> x `cmpSeason` s) dates)

run :: Database -> Season -> Year -> [Day] -> IO ()
run db s y dates = do
    migrate db

    print "Processing Teams"
    processTeams db teams

    case s of
        Playoffs -> processSeeds db (seeds y) -- get seeds from standings

    print "Processing Games"
    processGames db dates

    games <- selectGames db dates

    print "Processing Periods"
    processPeriods db games

    print "Processing Events"
    processEvents db games

    print "Processing Videos"
    processVideos db games

    return ()

isBootStrap :: [String] -> Bool
isBootStrap [] = False
isBootStrap (x:xs) = x == "-b"

main :: IO ()
main = do
    e <- env
    args <- getArgs
    day <- currentDay

    --tesitng only
    let day = dateFromComponents 2014 4 20

    let db = (database e)
    let s = (season e)
    let y = (year e)

    run db s y =<< case isBootStrap args of
        True -> bootstrap s y
        False -> dates s y day
