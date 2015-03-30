{-# LANGUAGE DeriveGeneric #-}

module Hockey.Types (
    AMPM(..),
    Season(..),
    fromSeason,
    toSeason,
    Game(..),
    Results(..),
    GameState(..),
    fromGameState,
    toGameState
) where

import GHC.Generics
import Data.UTC

data AMPM = AM | PM deriving (Enum, Show, Eq)

data Season = Preseason | Season | Playoffs deriving (Enum, Show, Eq)

fromSeason :: Season -> Integer
fromSeason Preseason = 1
fromSeason Season = 2
fromSeason Playoffs = 3

toSeason :: Integer -> Season
toSeason 1 = Preseason
toSeason 2 = Season
toSeason 3 = Playoffs

data Game = Game {
    id :: Integer,
    awayId :: String,
    homeId :: String,
    caTV :: String,
    usTV :: String,
    gameState :: GameState,
    awayScore :: Integer,
    homeScore :: Integer,
    awaySOG :: Integer,
    homeSOG :: Integer,
    gameTime :: Time,
    periodTime :: String
} deriving (Show, Generic)

data Results = Results {
    games :: [Game],
    currentDate :: Date,
    nextDate :: Date,
    prevDate :: Date
} deriving (Show, Generic)

data GameState = None | Before | Ongoing | Overtime | Final deriving (Enum, Show, Eq, Generic)

fromGameState :: GameState -> Integer
fromGameState None = 1
fromGameState Before = 2
fromGameState Ongoing = 3
fromGameState Overtime = 4
fromGameState Final = 4

toGameState :: Integer -> GameState
toGameState 1 = None
toGameState 2 = Before
toGameState 3 = Ongoing
toGameState 4 = Overtime
toGameState 5 = Final

-- data Scoreboard = Scoreboard {
--     id :: Integer,
--     awayId :: String,
--     homeId :: String,
--     caTV :: String,
--     usTV :: String,
--     gameState :: GameState,
--     awayScore :: Integer,
--     homeScore :: Integer,
--     awaySOG :: Integer,
--     homeSOG :: Integer
-- } deriving (Show, Generic)
