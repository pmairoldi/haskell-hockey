module Hockey.Database.Formatting (
    processGames
)

where

import Hockey.Types as T
import Hockey.Database.Types as DB
import Data.Time.Calendar
-- import Data.Time.LocalTime

gameToDB :: T.Game -> Day -> DB.Game
gameToDB game date = DB.Game (gameId game) (awayId game) (homeId game) date (T.gameTime game) (caTV game) (usTV game) (T.gameState game) (periodTime game) (awayScore game) (homeScore game) (awaySog game) (homeSog game)

processGames :: [T.Game] -> Day -> [DB.Game]
processGames [] _ = []
processGames (x:xs) d = [(gameToDB x d)] ++ (processGames xs d)
