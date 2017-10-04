module Handlers.Playoffs
  ( getPlayoffsR
  , getPlayoffsWithYearR
  ) where

import Data.Maybe
import Hockey.Database
import Hockey.Environment
import Hockey.Formatting hiding (year)
import Hockey.Types (Season(..))
import Models.Json
import Yesod

getPlayoffsR :: HandlerT site IO Value
getPlayoffsR = getPlayoffs Nothing

getPlayoffsWithYearR :: Integer -> HandlerT site IO Value
getPlayoffsWithYearR year = getPlayoffs $ Just (seasonYears year)

getYear :: Maybe Year -> Environment -> Year 
getYear sentYear env = fromMaybe (year env) sentYear

getPlayoffs :: Maybe Year -> HandlerT site IO Value
getPlayoffs sentYear =
  liftIO $ do
    e <- env
    
    let year = getYear sentYear e

    p <- selectPeriods (database e) year Playoffs
    s <- selectSeeds (database e) year
    g <- selectGamesForSeason (database e) year Playoffs
    e <- selectEvents (database e) year Playoffs
    returnJson $ PlayoffsResponse p s g e
