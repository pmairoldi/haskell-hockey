{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hockey.Types.Standings (
    StandingsType(..),
    ConferenceType(..),
    DivisionType(..),
    StandingsTeam(..),
    StandingTeamRecord(..),
    Standing(..),
    Standings(..),
    fromStandingsType,
    toStandingsType,
    fromConferenceType,
    toConferenceType,
    fromDivisionType,
    toDivisionType
)

where

import GHC.Generics
import Database.Persist.TH
import Hockey.Formatting
import Data.Aeson
import Control.Applicative as Applicative
import Data.Text

data StandingsType = WildCard | DivisionLeaders deriving (Enum, Show, Read, Eq, Generic)

data ConferenceType = Eastern | Western deriving (Enum, Show, Read, Eq, Generic)

data DivisionType = Metropolitan | Atlantic | Central | Pacific deriving (Enum, Show, Read, Eq, Generic)

-- StandingsTeam
data StandingsTeam = StandingsTeam {
    abbreviation :: String,
    division :: DivisionType,
    conference :: ConferenceType
} deriving (Show, Generic)

instance FromJSON StandingsTeam where
    parseJSON (Object v) = parseStandingsTeam v
    parseJSON _          = Applicative.empty

parseStandingsTeam v = do
    abbreviation <- fmap unpackToLower (v .: "abbreviation")
    c <-  v .: "conference"
    conference <- fmap toConferenceType (c .: "name")
    d <-  v .: "division"
    division <- fmap toDivisionType (d .: "name")

    return $ StandingsTeam abbreviation division conference

-- StandingTeamRecord
data StandingTeamRecord = StandingTeamRecord {
    team :: StandingsTeam,
    goalsAgainst :: Int,
    goalsScored :: Int,
    points :: Int,
    divisionRank :: Int,
    conferenceRank :: Int,
    leagueRank :: Int,
    wildCardRank :: Int,
    row :: Int,
    gamesPlayed :: Int
} deriving (Show, Generic)

instance FromJSON StandingTeamRecord where
    parseJSON (Object v) = parseStandingTeamRecord v
    parseJSON _          = Applicative.empty

parseStandingTeamRecord v = StandingTeamRecord <$>
    (v .: "team") <*>
    (v .: "goalsAgainst") <*>
    (v .: "goalsScored") <*>
    (v .: "points") <*>
    fmap valueToInt (v .: "divisionRank") <*>
    fmap valueToInt (v .: "conferenceRank") <*>
    fmap valueToInt (v .: "leagueRank") <*>
    fmap valueToInt (v .: "wildCardRank") <*>
    (v .: "row") <*>
    (v .: "gamesPlayed")

-- Standing
data Standing = Standing {
    standingsType :: StandingsType,
    teamRecords :: [StandingTeamRecord]
} deriving (Show, Generic)

instance FromJSON Standing where
    parseJSON (Object v) = parseStanding v
    parseJSON _          = Applicative.empty

parseStanding v = Standing <$>
    fmap toStandingsType (v .: "standingsType") <*>
    (v .: "teamRecords")

-- Standings
newtype Standings = Standings{records :: [Standing]} deriving (Show, Generic)

instance FromJSON Standings where
    parseJSON (Object v) = parseStandings v
    parseJSON _          = Applicative.empty

parseStandings v = Standings <$>
    (v .: "records")

fromStandingsType :: StandingsType -> String
fromStandingsType WildCard = "wildCard"
fromStandingsType DivisionLeaders = "divisionLeaders"

toStandingsType :: String -> StandingsType
toStandingsType "wildCard" = WildCard
toStandingsType "divisionLeaders" = DivisionLeaders
toStandingsType _ = WildCard

fromConferenceType :: ConferenceType -> String
fromConferenceType Eastern = "Eastern"
fromConferenceType Western = "Western"

toConferenceType :: String -> ConferenceType
toConferenceType "Eastern" = Eastern
toConferenceType "Western" = Western
toConferenceType _ = Eastern

fromDivisionType :: DivisionType -> String
fromDivisionType Metropolitan = "Metropolitan"
fromDivisionType Atlantic = "Atlantic"
fromDivisionType Central = "Central"
fromDivisionType Pacific = "Pacific"

toDivisionType :: String -> DivisionType
toDivisionType "Metropolitan" = Metropolitan
toDivisionType "Atlantic" = Atlantic
toDivisionType "Central" = Central
toDivisionType "Pacific" = Pacific
toDivisionType _ = Metropolitan
