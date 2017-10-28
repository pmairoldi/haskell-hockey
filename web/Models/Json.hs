{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.Json
  ( Matchup(..)
  , Game(..)
  , Period(..)
  , Event(..)
  , Bracket(..)
  , toMatchup
  ) where

import Data.Char as Char
import Data.List as List
import Data.Text
import Hockey.Database hiding (Team(..))
import Hockey.Formatting
       (formattedGame, formattedSeason, formattedYear, intToInteger,
        fromStrength, fromEventType, boolToInt)
import Hockey.Types (Season(..), Team(..))
import Hockey.Teams
import Yesod
import Data.Maybe

-- PlayoffSeed
instance ToJSON PlayoffSeed where
  toJSON PlayoffSeed {..} =
    object
      [ "seasonId" .=
        (formattedYear (intToInteger playoffSeedYear) ++
         formattedSeason Playoffs)
      , "conference" .= playoffSeedConference
      , "seed" .= playoffSeedSeries
      , "homeId" .= playoffSeedHomeId
      , "awayId" .= playoffSeedAwayId
      , "round" .= playoffSeedRound
      ]

-- Game
instance ToJSON Game where
  toJSON Game {..} =
    object
      [ "seasonId" .=
        (formattedYear (intToInteger gameYear) ++ formattedSeason gameSeason)
      , "awayId" .= gameAwayId
      , "homeId" .= gameHomeId
      , "awayScore" .= gameAwayScore
      , "homeScore" .= gameHomeScore
      , "gameId" .= show gameGameId
      , "date" .= show gameDate
      , "time" .= show gameTime
      , "tv" .= gameTv
      , "period" .= gamePeriod
      , "periodTime" .= List.map Char.toUpper gamePeriodTime
      , "homeStatus" .= gameHomeStatus
      , "awayStatus" .= gameAwayStatus
      , "homeHighlight" .= gameHomeHighlight
      , "awayHighlight" .= gameAwayHighlight
      , "homeCondense" .= gameHomeCondense
      , "awayCondense" .= gameAwayCondense
      , "active" .= gameActive
      ]

-- Period
instance ToJSON Period where
  toJSON Period {..} =
    object
      [ "teamId" .= periodTeamId
      , "gameId" .= show periodGameId
      , "period" .= periodPeriod
      , "goals" .= periodGoals
      , "shots" .= periodShots
      ]

-- Event
instance ToJSON Event where
  toJSON Event {..} =
    object
      [ "eventId" .= eventEventId
      , "gameId" .= show eventGameId
      , "teamId" .= eventTeamId
      , "period" .= eventPeriod
      , "time" .= eventTime
      , "type" .= fromEventType eventEventType
      , "description" .= eventDescription
      , "videoLink" .= eventVideoLink
      , "formalId" .= eventFormalId
      , "strength" .= fromStrength eventStrength
      ]

-- Team
instance ToJSON Team where
  toJSON Team {..} =
    object
      [ "abbreviation" .= abr
      , "city" .= city
      , "name" .= name
      , "color" .= color
      ]


data Matchup = Matchup
  { id :: String
  , topTeam :: Team
  , bottomTeam :: Team
  , seed :: Int
  , round :: Int
  , games :: [Game]
  } deriving (Show)

team :: String -> Team
team abbreaviation = fromMaybe (Team abbreaviation "" "" "") (teamByAbbreviation abbreaviation)

seriesId :: PlayoffSeed -> Int
seriesId seed = case playoffSeedConference seed of 
  "w" -> case playoffSeedRound seed of
    1 -> playoffSeedSeries seed + 4
    2 -> playoffSeedSeries seed + 2
    3 -> playoffSeedSeries seed + 1
    _ -> playoffSeedSeries seed
  _ -> playoffSeedSeries seed

matchUpId :: PlayoffSeed -> String 
matchUpId seed = formattedYear (intToInteger (playoffSeedYear seed)) 
  ++ formattedSeason Playoffs 
  ++ "0" 
  ++ show (playoffSeedRound seed)
  ++ show (seriesId seed)

filterMatchupGames :: PlayoffSeed -> Game -> Bool
filterMatchupGames seed game = matchUpId seed `List.isPrefixOf` show (gameGameId game)

toMatchup :: [Game] -> PlayoffSeed -> Matchup
toMatchup games seed =
  Matchup
    (matchUpId seed)
    (team (playoffSeedHomeId seed))
    (team (playoffSeedAwayId seed))
    (seriesId seed)
    (playoffSeedRound seed)
    (List.filter (filterMatchupGames seed) games)

instance ToJSON Matchup where
  toJSON Matchup {..} =
    object
      [ "id" .= pack id
      , "topTeam" .= topTeam
      , "bottomTeam" .= bottomTeam
      , "seed" .= seed
      , "round" .= round
      , "games" .= games
      ]

data Bracket = Bracket
  { year :: String
  , matchups :: [Matchup]
  } deriving (Show)

instance ToJSON Bracket where
  toJSON Bracket {..} = object ["year" .= pack year, "matchups" .= matchups]
