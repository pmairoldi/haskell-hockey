{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Hockey.Types (
    AMPM(..),
    Season(..),
    Game(..),
    Results(..),
    GameState(..),
    GameDate(..),
    GameDates(..),
    fromGameState,
    toGameState,
    fromSeason,
    toSeason,
) where

import GHC.Generics
import Data.Time.Calendar
import Data.Time.LocalTime
import Database.Persist.TH

data AMPM = AM | PM deriving (Enum, Show, Eq)

data Season = Preseason | Season | Playoffs deriving (Enum, Show, Read, Eq, Generic)

data GameState = None | Before | Ongoing | Overtime | Final deriving (Enum, Show, Read, Eq, Generic)
derivePersistField "GameState"

data Game = Game {
    gameId :: Int,
    awayId :: String,
    homeId :: String,
    caTV :: String,
    usTV :: String,
    gameState :: GameState,
    awayScore :: Int,
    homeScore :: Int,
    awaySog :: Int,
    homeSog :: Int,
    gameTime :: TimeOfDay,
    periodTime :: String
} deriving (Show, Generic)

data Results = Results {
    games :: [Game],
    currentDate :: Day,
    nextDate :: Day,
    prevDate :: Day
} deriving (Show, Generic)

data GameDate = GameDate {
    date :: Day,
    season :: Season,
    gameNumber :: Int
} deriving (Show, Generic)

data GameDates = GameDates {
    dates :: [GameDate]
} deriving (Show, Generic)

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

-- Conversion Functions
fromSeason :: Season -> Integer
fromSeason Preseason = 1
fromSeason Season = 2
fromSeason Playoffs = 3

toSeason :: Integer -> Season
toSeason 1 = Preseason
toSeason 2 = Season
toSeason 3 = Playoffs

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
