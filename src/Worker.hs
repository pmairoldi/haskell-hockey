import Hockey.Database
import Hockey.Processing
import Hockey.Formatting hiding (season, year, Game, dates)
import Hockey.Environment
import System.Environment (getArgs)
import Data.List as List

processGames :: Database -> [Day] -> IO ()
processGames db [] = return ()
processGames db (x:xs) = do
    values <- getGames [x]
    db `process` (upsertMany values)
    processGames db xs

processEvents :: Database -> [Game] -> IO ()
processEvents db [] = return ()
processEvents db (x:xs) = do
    values <- getEvents [x]
    db `process` (upsertMany values)
    processEvents db xs

processVideos :: Database -> [Game] -> IO ()
processVideos db [] = return ()
processVideos db (x:xs) = do
        values <- getVideos [x]
        db `process` (upsertMany values)
        processVideos db xs

dates :: Season -> Year -> IO [Day]
dates s y = do
    --filter based on today's date also
    dates <- getDates (months s y)
    return $ List.map date (filter (\x -> x `cmpSeason` s) dates)

bootstrap :: Season -> Year -> IO [Day]
bootstrap s y= do
        dates <- getDates (months s y)
        return $ List.map date (filter (\x -> x `cmpSeason` s) dates)

run :: Database -> Season -> Year -> [Day] -> IO ()
run db s y dates = do
    migrate db

    processGames db dates

    let games = []

    processEvents db games
    processVideos db games

    return ()

isBootStrap :: [String] -> Bool
isBootStrap [] = False
isBootStrap (x:xs) = x == "-b"

main :: IO ()
main = do
    e <- env
    args <- getArgs

    let db = (database e)
    let s = (season e)
    let y = (year e)

    run db s y =<< case isBootStrap args of
        True -> bootstrap s y
        False -> dates s y
