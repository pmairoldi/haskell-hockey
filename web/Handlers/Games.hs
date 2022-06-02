module Handlers.Games
  ( getGameR
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

getGameR :: Int -> HandlerFor site Value
getGameR gameId = getGame gameId

getGame :: Int -> HandlerFor site Value
getGame gameId =
  liftIO $ do
    e <- env
    g <- selectGame (database e) gameId
    p <- selectGamePeriods (database e) gameId
    e <- selectGameEvents (database e) gameId
    returnJson $ (p, g, e)
