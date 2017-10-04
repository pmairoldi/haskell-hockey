module Handlers.Playoffs
  ( getPlayoffsR
  ) where

import Hockey.Database
import Hockey.Environment
import Models.Json
import Yesod

getPlayoffsR :: HandlerT site IO Value
getPlayoffsR =
  liftIO $ do
    e <- env
    p <- selectPeriods (database e) (year e) (season e)
    s <- selectSeeds (database e) (year e)
    g <- selectGamesForSeason (database e) (year e) (season e)
    e <- selectEvents (database e) (year e) (season e)
    returnJson $ PlayoffsResponse p s g e
