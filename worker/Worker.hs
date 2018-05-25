import Control.Monad (when)
import Data.List as List
import Data.Time hiding (months)
import Hockey.Environment
import Hockey.Formatting hiding (dates, season, year)
import Hockey.Playoffs hiding (year)
import Hockey.Processing
import System.Environment (getArgs)

currentDay :: IO Day
currentDay = do
  c <- getCurrentTime
  return $ utctDay c

dates :: Season -> Year -> Day -> Integer -> IO (Day, Day)
dates s y day range = return (addDays (-range) day, addDays range day)

bootstrap :: Season -> Year -> IO (Day, Day)
bootstrap s y = return $ days s y

logMsg
  :: Show a
  => a -> LoggingType -> IO ()
logMsg msg loggingType = do
  e <- env
  case (logType e, loggingType) of
    (Debug, Debug) -> print msg
    (Debug, Info) -> print msg
    (Info, Info) -> print msg
    _ -> return ()

run :: Database -> Season -> Year -> (Day, Day) -> IO ()
run db s y dates = do
  startTime <- getCurrentTime
  logMsg "Processing Games" Debug
  uncurry (processGames db) dates
  logMsg "Fetch Games" Debug
  games <- uncurry (selectGames db) dates
  logMsg "Processing Events" Debug
  processEvents db games
  -- case s of
  --   Playoffs -> do
  --     logMsg "Processing Seeds" Debug
  --     processSeeds db y
  --     processSeries db y
  --     processSeries db y
  --     processSeries db y
  endTime <- getCurrentTime
  logMsg (diffUTCTime endTime startTime) Info
  return ()

isBootStrap :: [String] -> Bool
isBootStrap [] = False
isBootStrap (x:xs) = x == "-b"

datesToProcess :: [String] -> Season -> Year -> Integer -> IO (Day, Day)
datesToProcess args s y r = do
  day <- currentDay
  if isBootStrap args
    then bootstrap s y
    else dates s y day r

bootstrapDatabase :: Database -> Year -> Season -> IO ()
bootstrapDatabase db y s = do
  logMsg "Processing Teams" Debug
  processTeams db teamList
  logMsg "Processing Seeds" Debug
  processSeeds db y
  logMsg "Processing Series" Debug
  processSeries db y
  processSeries db y
  processSeries db y
  deleteEvents db y s

main :: IO ()
main = do
  e <- env
  args <- getArgs
  let db = database e
  let s = season e
  let y = year e
  let r = intToInteger (range e)
  migrate db
  Control.Monad.when (isBootStrap args) $ bootstrapDatabase db y s
  run db s y =<< datesToProcess args s y r
