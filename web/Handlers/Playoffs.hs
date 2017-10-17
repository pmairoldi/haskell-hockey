module Handlers.Playoffs
  ( getPlayoffsR
  , getPlayoffsWithYearR
  ) where

import Data.List
import Data.Maybe
import Hockey.Database
import Hockey.Environment
import Hockey.Formatting hiding (year)
import Hockey.Types (Season(..))
import Models.Json
import Models.Path (Year(..))
import Yesod

getPlayoffsR :: HandlerT site IO Value
getPlayoffsR = getPlayoffs Nothing

getPlayoffsWithYearR :: Models.Path.Year -> HandlerT site IO Value
getPlayoffsWithYearR year = getPlayoffs $ Just (start year, end year)

getYear :: Maybe Hockey.Formatting.Year -> Environment -> Hockey.Formatting.Year
getYear sentYear env = fromMaybe (Hockey.Environment.year env) sentYear

getPlayoffs :: Maybe Hockey.Formatting.Year -> HandlerT site IO Value
getPlayoffs sentYear =
  liftIO $ do
    e <- env
    let year = getYear sentYear e
    let yearString = show (fst year) ++ show (snd year)
    -- p <- selectPeriods (database e) year Playoffs
    seeds <- selectSeeds (database e) year
    games <- selectGamesForSeason (database e) year Playoffs
    -- e <- selectEvents (database e) year Playoffs
    returnJson $ Bracket yearString $ map (toMatchup games) seeds
