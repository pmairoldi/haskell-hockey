{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hockey.Parsing (
    decodeResponse
) where

import Hockey.Formatting
import Data.Aeson
import Control.Applicative as Applicative
import Hockey.Types

decodeResponse :: (FromJSON a) => IO String -> IO (Maybe a)
decodeResponse response = do
    rsp <- response
    return $ decode (stringToLazyByteString rsp)

-- Season
instance FromJSON Season

-- GameState
instance FromJSON GameState

-- Game
instance FromJSON Game where
    parseJSON (Object v) = parseGame v
    parseJSON _          = Applicative.empty

parseGame v = Game <$>
    v .: "id" <*>
    fmap unpackToLower (v .: "ata") <*>
    fmap unpackToLower (v .: "hta") <*>
    fmap splitAndJoin (v .: "canationalbroadcasts") <*>
    fmap splitAndJoin (v .: "usnationalbroadcasts") <*>
    fmap toGameState (v .:"gs") <*>
    fmap valueToInteger (v .: "ats") <*>
    fmap valueToInteger (v .: "hts") <*>
    fmap valueToInteger (v .:? "atsog") <*>
    fmap valueToInteger (v .:? "htsog") <*>
    fmap unpackParseTime (v .: "bs") <*>
    fmap removeGameTime (v .: "bs")

-- Results
instance FromJSON Results where
    parseJSON (Object v) = parseResults v
    parseJSON _          = Applicative.empty

parseResults v = Results <$>
    v .: "games" <*>
    fmap unpackParseDate (v .: "currentDate") <*>
    fmap unpackParseDate (v .: "nextDate") <*>
    fmap unpackParseDate (v .: "prevDate")

-- GameDate
instance FromJSON GameDate where
    parseJSON (Object v) = parseGameDate v
    parseJSON _          = Applicative.empty

parseGameDate v = GameDate <$>
    fmap unpackParseDate (v .: "gd") <*>
    fmap toSeason (v .: "gt") <*>
    (v .: "n")

-- GameDates
instance FromJSON GameDates where
    parseJSON (Object v) = parseGameDates v
    parseJSON _          = Applicative.empty

parseGameDates v = GameDates <$>
    v .: "gameDates"

-- EventType
instance FromJSON EventType

-- Strength
instance FromJSON Strength

-- Event
instance FromJSON Event where
    parseJSON (Object v) = parseEvent v
    parseJSON _          = Applicative.empty

parseEvent v = Event <$>
        v .: "eventid" <*>
        v .: "teamid" <*>
        v .: "period" <*>
        v .: "time" <*>
        v .: "desc" <*>
        v .: "formalEventId" <*>
        v .: "strength" <*>
        v .: "type"

-- EventPlays
instance FromJSON EventPlays where
    parseJSON (Object v) = parseEventPlays v
    parseJSON _          = Applicative.empty

parseEventPlays v = EventPlays <$>
        v .: "play"

-- EventGame
instance FromJSON EventGame where
    parseJSON (Object v) = parseEventGame v
    parseJSON _          = Applicative.empty

parseEventGame v = EventGame <$>
        v .: "awayteamid" <*>
        v .: "hometeamid" <*>
        -- v .: "awayteamnick" <*>
        -- v .: "hometeamnick" <*>
        v .: "plays"

-- EventData
instance FromJSON EventData where
    parseJSON (Object v) = parseEventData v
    parseJSON _          = Applicative.empty

parseEventData v = EventData <$>
        v .: "refreshInterval" <*>
        v .: "game"

-- GameEvents
instance FromJSON GameEvents where
    parseJSON (Object v) = parseGameEvents v
    parseJSON _          = Applicative.empty

parseGameEvents v = GameEvents <$>
        v .: "data"
