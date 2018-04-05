{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hockey.Types.Events (
    Events(..),
    LiveData(..),
    Plays(..),
    Play(..),
    playTeam,
    playStrength
)

where

import GHC.Generics
import Database.Persist.TH
import Hockey.Formatting hiding (toStrength, strength, team, status)
import Data.Aeson
import Control.Applicative as Applicative
import Data.Text

-- PlayTeam
newtype PlayTeam = PlayTeam{abbreviation :: String} deriving (Show, Generic)

instance FromJSON PlayTeam where
    parseJSON (Object v) = parsePlayTeam v
    parseJSON _          = Applicative.empty

parsePlayTeam v = PlayTeam <$>
    fmap unpackToLower (v .: "triCode")

-- PlayStatus
newtype PlayStatus = PlayStatus{strength :: Strength} deriving (Show, Generic)

instance FromJSON PlayStatus where
    parseJSON (Object v) = parsePlayStatus v
    parseJSON _          = Applicative.empty

parsePlayStatus v = PlayStatus <$>
    fmap toStrength (v .: "code")

-- Play
data Play = Play {
   eventId :: Integer,
   period :: Integer,
   time :: String,
   eventType :: EventType,
   description :: String, 
   formalId :: String,
   status :: Maybe PlayStatus,
   team :: Maybe PlayTeam
} deriving (Show, Generic)

instance FromJSON Play where
    parseJSON (Object v) = parsePlay v
    parseJSON _          = Applicative.empty

parsePlay v =  do
    about <- v .: "about"

    eventId <- about .: "eventIdx"
    period <- about .: "period"
    time <- about .: "periodTime"

    result <- v .: "result"

    eventType <- fmap (Hockey.Types.Events.toEventType . unpackToLower) (result .: "eventTypeId")
    description <- result .: "description"
    formalId <- result .: "eventCode"
    strength <- result .:? "strength"

    team <- v .:? "team"

    return $ Play eventId period time eventType description formalId strength team

-- Plays
newtype Plays = Plays{allPlays :: [Play]} deriving (Show, Generic)

instance FromJSON Plays where
    parseJSON (Object v) = parsePlays v
    parseJSON _          = Applicative.empty

parsePlays v = Plays <$>
    (v .: "allPlays")

-- LiveData
newtype LiveData = LiveData{plays :: Plays} deriving (Show, Generic)

instance FromJSON LiveData where
    parseJSON (Object v) = parseLiveData v
    parseJSON _          = Applicative.empty

parseLiveData v = LiveData <$>
    (v .: "plays")

-- Events
newtype Events = Events{liveData :: LiveData} deriving (Show, Generic)

instance FromJSON Events where
    parseJSON (Object v) = parseEvents v
    parseJSON _          = Applicative.empty

parseEvents v = Events <$>
    (v .: "liveData")


playTeam :: Play -> String 
playTeam x = case team x of 
    Just y -> abbreviation y
    Nothing -> ""

playStrength :: Play -> Strength 
playStrength x = case status x of 
    Just y -> strength y
    Nothing -> Normal

toEventType :: String -> EventType
toEventType "shot" = Shot
toEventType "hit" = Hit
toEventType "penalty" = Penalty
toEventType "goal" = Goal
toEventType "fight" = Fight
toEventType _ = Unknown

toStrength :: String -> Strength
toStrength "EVEN" = Normal
toStrength "PPG" = Powerplay
toStrength "SHG" = Shorthand
toStrength _ = Normal
