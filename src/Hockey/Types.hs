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
    EventType(..),
    Event(..),
    EventPlays(..),
    EventGame(..),
    EventData(..),
    GameEvents(..),
    Strength(..),
    fromGameState,
    toGameState,
    fromSeason,
    toSeason,
    fromEventType,
    toEventType,
    fromStrength,
    toStrength
) where

import GHC.Generics
import Data.Time.Calendar
import Data.Time.LocalTime
import Database.Persist.TH

data AMPM = AM | PM deriving (Enum, Show, Eq)

data Season = Preseason | Season | Playoffs deriving (Enum, Show, Read, Eq, Generic)

data GameState = None | Before | Ongoing | Overtime | Final deriving (Enum, Show, Read, Eq, Generic)
derivePersistField "GameState"

data Strength = Normal | Powerplay | Shorthand deriving (Enum, Show, Read, Eq, Generic)
derivePersistField "Strength"

data EventType = Unknown | Shot | Hit | Penalty | Goal | Fight deriving (Enum, Show, Read, Eq, Generic)
derivePersistField "EventType"

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

-- convert strenth to type
data Event = Event {
    eventId :: Int,
    teamId :: Int,
    period :: Int,
    time :: String,
    description :: String,
    formalId :: String,
    strength :: Strength,
    eventType :: EventType
} deriving (Show, Generic)

data EventPlays = EventPlays {
    play :: [Event]
} deriving (Show, Generic)

data EventGame = EventGame {
    awayTeamId :: Int,
    homeTeamId :: Int,
    awayName :: String,
    homeName :: String,
    plays :: EventPlays
} deriving (Show, Generic)

data EventData = EventData {
    refreshInterval :: Int,
    game :: EventGame
} deriving (Show, Generic)

data GameEvents = GameEvents {
    eventData :: EventData
} deriving (Show, Generic)

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

fromEventType :: EventType -> String
fromEventType Shot = "shot"
fromEventType Hit = "hit"
fromEventType Penalty = "penalty"
fromEventType Goal = "goal"
fromEventType Fight = "fight"
fromEventType Unknown = ""

toEventType :: String -> EventType
toEventType "shot" = Shot
toEventType "hit" = Hit
toEventType "penalty" = Penalty
toEventType "goal" = Goal
toEventType "fight" = Fight
toEventType _ = Unknown

-- check theese
fromStrength :: Strength -> Int
fromStrength Normal = 701
fromStrength Powerplay = 702
fromStrength Shorthand = 703

toStrength :: Int -> Strength
toStrength 701 = Normal
toStrength 702 = Powerplay
toStrength 703 = Shorthand
toStrength _ = Normal
