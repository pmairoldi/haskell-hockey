{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.LegacyJson
  ( Period(..)
  , PlayoffSeed(..)
  , Game(..)
  , Event(..)
  , PlayoffsResponse(..)
  ) where

import Data.Char as Char
import Data.List as List
import Hockey.Database
import Hockey.Formatting
       (formattedGame, formattedSeason, formattedYear, intToInteger,
        fromStrength, fromEventType, boolToInt)
import Hockey.Types (Season(..), GameState(..))
import Yesod

timeOrTBD time state
  | state == TBD = ""
  | otherwise = show time

fixPeriod period state
  | state == TBD = 0
  | state == Hockey.Types.None = 0
  | state == Before = 0
  | otherwise = period

fixStatus status state
  | state == Ended = ""
  | state == Final = ""
  | otherwise = status

-- Period
instance ToJSON Period where
  toJSON Period {..} =
    object
      [ "teamID" .= periodTeamId
      , "gameID" .= show periodGameId
      , "period" .= periodPeriod
      , "goals" .= periodGoals
      , "shots" .= periodShots
      ]

-- Seeds
instance ToJSON PlayoffSeed where
  toJSON PlayoffSeed {..} =
    object
      [ "seasonID" .=
        (formattedYear (intToInteger playoffSeedYear) ++
         formattedSeason Playoffs)
      , "conference" .= playoffSeedConference
      , "seed" .= playoffSeedSeries
      , "homeID" .= playoffSeedHomeId
      , "awayID" .= playoffSeedAwayId
      , "round" .= playoffSeedRound
      ]

-- Team
instance ToJSON Game where
  toJSON Game {..} =
    object
      [ "seasonID" .=
        (formattedYear (intToInteger gameYear) ++ formattedSeason gameSeason)
      , "awayID" .= gameAwayId
      , "homeID" .= gameHomeId
      , "awayScore" .= gameAwayScore
      , "homeScore" .= gameHomeScore
      , "gameID" .= show gameGameId
      , "date" .= show gameDate
      , "time" .= timeOrTBD gameTime gameState
      , "tv" .= gameTv
      , "period" .= fixPeriod gamePeriod gameState
      , "periodTime" .= List.map Char.toUpper gamePeriodTime
      , "homeStatus" .= fixStatus gameHomeStatus gameState
      , "awayStatus" .= fixStatus gameAwayStatus gameState
      , "homeHighlight" .= gameHomeHighlight
      , "awayHighlight" .= gameAwayHighlight
      , "homeCondense" .= gameHomeCondense
      , "awayCondense" .= gameAwayCondense
      , "active" .= gameActive
      ]

-- Event
instance ToJSON Event where
  toJSON Event {..} =
    object
      [ "eventID" .= eventEventId
      , "gameID" .= show eventGameId
      , "teamID" .= eventTeamId
      , "period" .= eventPeriod
      , "time" .= eventTime
      , "type" .= fromEventType eventEventType
      , "description" .= eventDescription
      , "videoLink" .= eventVideoLink
      , "formalID" .= eventFormalId
      , "strength" .= fromStrength eventStrength
      ]

data PlayoffsResponse = PlayoffsResponse
  { periods :: [Period]
  , seeds :: [PlayoffSeed]
  , games :: [Game]
  , events :: [Event]
  } deriving (Show)

instance ToJSON PlayoffsResponse where
  toJSON PlayoffsResponse {..} =
    object
      [ "periods" .= periods
      , "teams" .= seeds
      , "games" .= games
      , "events" .= events
      ]
