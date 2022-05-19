{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Hockey.Types (
    Year(..),
    Team(..),
    HomeAway(..),
    AMPM(..),
    Season(..),
    GameState(..),
    Strength(..),
    EventType(..),
    TeamData(..),
    TeamInfo(..),
    Teams(..),
    Broadcast(..),
    PeriodData(..),
    Period(..),
    ScoreInfo(..),
    ScoreTeams(..),
    Linescore(..),
    State(..),
    GameDateTime(..),
    Game(..),
    GameDates(..),
    Results(..),
    Event(..),
    EventPlays(..),
    EventGame(..),
    EventData(..),
    GameEvents(..),
    ScoreboardData(..),
    Scoreboard(..),
    VideoLink(..),
    LinkType(..),
    MediaItem(..),
    Media(..),
    Content(..),
    VideoQuality(..),
    VideoType(..),
    toVideoType,
    toVideoQuality,
    fromGameState,
    toGameState,
    fromSeason,
    toSeason,
    fromEventType,
    toEventType,
    fromStrength,
    toStrength,
    fromHomeAway,
    toHomeAway
) where

import GHC.Generics
import Data.Time.Calendar
import Data.Time.LocalTime
import Database.Persist.TH

type Year = (Integer, Integer)

data Team = Team {
    abr :: String,
    city :: String,
    name :: String,
    color :: String
} deriving (Show)

data HomeAway = Home | Away deriving (Enum, Show, Eq)

data AMPM = AM | PM deriving (Enum, Show, Eq)

data VideoType = Condense | Recap | Other deriving (Enum, Show, Read, Eq, Generic)

data VideoQuality = Mobile | Tablet | Tablet60 | Wired | Wired60 | WiredWeb | Flash192 | Flash450 | Flash1200 | Flash1800 | Undefined deriving (Enum, Show, Read, Eq, Generic)

data Season = Preseason | Season | Playoffs deriving (Enum, Show, Read, Eq, Generic)
derivePersistField "Season"

data GameState = None | Before | Ongoing | Overtime | Final | TBD deriving (Enum, Show, Read, Eq, Generic)
derivePersistField "GameState"

data Strength = Normal | Powerplay | Shorthand deriving (Enum, Show, Read, Eq, Generic)
derivePersistField "Strength"

data EventType = Unknown | Shot | Hit | Penalty | Goal | Fight deriving (Enum, Show, Read, Eq, Generic)
derivePersistField "EventType"

data TeamData = TeamData {
    teamAbr :: String
} deriving (Show, Generic)

data TeamInfo = TeamInfo {
    team :: TeamData
} deriving (Show, Generic)

data Teams = Teams {
    awayInfo :: TeamInfo,
    homeInfo :: TeamInfo
} deriving (Show, Generic)

data Broadcast = Broadcast {
    broadcastName :: String
} deriving (Show, Generic)

data PeriodData = PeriodData {
    periodGoals :: Int,
    periodShots :: Int
} deriving (Show, Generic)

data Period = Period {
    periodId :: Int,
    homePeriod :: PeriodData,
    awayPeriod :: PeriodData
} deriving (Show, Generic)

data ScoreInfo = ScoreInfo {
    goals :: Int,
    shots :: Int,
    powerPlay :: Bool,
    goaliePulled :: Bool
} deriving (Show, Generic)

data ScoreTeams = ScoreTeams {
    homeTeam :: ScoreInfo,
    awayTeam :: ScoreInfo
} deriving (Show, Generic)

data Linescore = Linescore {
    gamePeriods :: [Period],
    gamePeriod :: Int,
    periodTime :: String,
    scoreTeams :: ScoreTeams
} deriving (Show, Generic)

data State = State {
    gameState :: GameState
} deriving (Show, Generic)

data GameDateTime = GameDateTime {
    gameDay :: Day,
    gameTime :: TimeOfDay
} deriving (Show, Generic)

data VideoLink = VideoLink {
    quality :: VideoQuality,
    url :: String
} deriving (Show, Generic)

data LinkType = LinkType {
    playbacks :: [VideoLink]
} deriving (Show, Generic)

data MediaItem = MediaItem {
    videoType :: VideoType,
    itemTypes :: [LinkType]
} deriving (Show, Generic)

data Media = Media {
    items :: [MediaItem]
} deriving (Show, Generic)

data Content = Content {
    media :: Maybe Media
} deriving (Show, Generic)

data Game = Game {
    date :: GameDateTime,
    season :: Season,
    gameId :: Int,
    teams :: Teams,
    broadcasts :: [Broadcast],
    linescore :: Linescore,
    status :: State,
    content :: Content
} deriving (Show, Generic)

data GameDates = GameDates {
    games :: [Game]
} deriving (Show, Generic)

data Results = Results {
    dates :: [GameDates]
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
    game :: EventGame
} deriving (Show, Generic)

data GameEvents = GameEvents {
    eventData :: EventData
} deriving (Show, Generic)

data ScoreboardData = ScoreboardData {
    periodTeamId :: String,
    periods :: [PeriodData]
} deriving (Show, Generic)

data Scoreboard = Scoreboard {
    home :: ScoreboardData,
    away :: ScoreboardData
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
fromGameState Final = 7
fromGameState TBD = 8

toGameState :: Integer -> GameState
toGameState 1 = None
toGameState 2 = Before
toGameState 3 = Ongoing
toGameState 4 = Overtime
toGameState 7 = Final
toGameState 8 = TBD
toGameState _ = None

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

fromHomeAway :: HomeAway -> String
fromHomeAway Home = "h"
fromHomeAway Away = "a"

toHomeAway :: String -> HomeAway
toHomeAway "h" = Home
toHomeAway "a" = Away
toHomeAway _ = Home

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

toVideoType :: String -> VideoType
toVideoType "Extended Highlights" = Condense
toVideoType "Recap" = Recap
toVideoType _ = Other

toVideoQuality :: String -> VideoQuality
toVideoQuality "HTTP_CLOUD_MOBILE" = Mobile
toVideoQuality "HTTP_CLOUD_TABLET" = Tablet
toVideoQuality "HTTP_CLOUD_TABLET_60" = Tablet60
toVideoQuality "HTTP_CLOUD_WIRED" = Wired
toVideoQuality "HTTP_CLOUD_WIRED_60" = Wired60
toVideoQuality "HTTP_CLOUD_WIRED_WEB" = WiredWeb
toVideoQuality "FLASH_192K_320X180" = Flash192
toVideoQuality "FLASH_450K_384x216" = Flash450
toVideoQuality "FLASH_1200K_640X360" = Flash1200
toVideoQuality "FLASH_1800K_896x504" = Flash1800
toVideoQuality _ = Undefined
