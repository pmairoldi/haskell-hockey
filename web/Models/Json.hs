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
import Hockey.Types (Season(..), Team(..), GameState(..))
import Hockey.Teams
import Yesod

timeOrTBD time state
  | state == TBD = ""
  | otherwise = show time

fixPeriod period state
  | state == TBD = 0
  | state == Hockey.Types.None = 0
  | state == Before = 0
  | otherwise = period

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
      [ "id" .= show gameGameId      
      , "awayTeam" .= GameTeam (teamByAbbreviation gameAwayId) gameAwayScore gameAwayStatus (emptyToMaybeString gameAwayCondense) (emptyToMaybeString gameAwayHighlight)
      , "homeTeam" .= GameTeam (teamByAbbreviation gameHomeId) gameHomeScore gameHomeStatus (emptyToMaybeString gameHomeCondense) (emptyToMaybeString gameHomeHighlight)
      , "period" .= fixPeriod gamePeriod gameState
      , "periodTime" .= List.map Char.toUpper gamePeriodTime
      , "date" .= show gameDate
      , "time" .= timeOrTBD gameTime gameState
      , "tv" .= gameTv
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

-- MatchupTeam 
data MatchupTeam = MatchupTeam {
  team :: Maybe Team,
  wins :: Int
} deriving (Show)

instance ToJSON MatchupTeam where
  toJSON MatchupTeam {..} =
    object
      [ "team" .= team
      , "wins" .= wins
      ]


-- GameTeam 
data GameTeam = GameTeam {
  gameTeam :: Maybe Team,
  score :: Int,
  status :: String,
  condense :: Maybe String,
  highlight :: Maybe String
} deriving (Show)

instance ToJSON GameTeam where
  toJSON GameTeam {..} =
    object
      [ "team" .= gameTeam
      , "score" .= score
      , "status" .= status
      , "condense" .= condense
      , "highlight" .= highlight
      ]

data Matchup = Matchup
  { id :: String
  , topTeam :: MatchupTeam
  , bottomTeam :: MatchupTeam
  , seed :: Int
  , round :: Int
  , games :: [Game]
  } deriving (Show)

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

awayTeamWon :: String -> Game -> Bool
awayTeamWon abbreviation game = (gameAwayId game == abbreviation) && (gameAwayScore game > gameHomeScore game)

homeTeamWon :: String -> Game -> Bool
homeTeamWon abbreviation game = (gameHomeId game == abbreviation) && (gameHomeScore game > gameAwayScore game)

hasWonGame :: String -> Game -> Bool 
hasWonGame abbreviation game = (awayTeamWon abbreviation game || homeTeamWon abbreviation game) && (gameState game == Final)

winsForTeam :: String -> [Game] -> Int
winsForTeam abbreviation games = List.length (List.filter (hasWonGame abbreviation) (List.filter gameActive games))

toMatchupTeam:: String -> [Game] -> MatchupTeam
toMatchupTeam abbreviation games = MatchupTeam (teamByAbbreviation abbreviation) (winsForTeam abbreviation games)

toMatchup :: [Game] -> PlayoffSeed -> Matchup
toMatchup games seed =
  Matchup
    (matchUpId seed)
    (toMatchupTeam (playoffSeedHomeId seed) matchupGames)
    (toMatchupTeam (playoffSeedAwayId seed) matchupGames)
    (seriesId seed)
    (playoffSeedRound seed)
    matchupGames
  where matchupGames = List.filter (filterMatchupGames seed) games

emptyToMaybeString :: String -> Maybe String 
emptyToMaybeString value = case List.length value of
  0 -> Nothing
  _ -> Just value

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
