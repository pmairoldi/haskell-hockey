{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hockey.Database.Types (
    migrate,
    Game(..),
    Event(..),
    Team(..),
    PlayoffSeed(..),
    Period(..),
    selectTimeForGame,
    selectGames,
    selectPeriods,
    selectSeeds,
    selectGamesForSeason,
    selectEvents,
    selectGamesForSeries,
    updateGamesToInactive
)

where

import Database.Persist.Postgresql hiding (migrate)
import Database.Persist.Sqlite hiding (migrate)
import Database.Persist.TH
import Hockey.Database.Internal hiding (None)
import Hockey.Types (GameState(..), EventType(..), Strength(..), Season(..), Year(..), AMPM(..))
import Hockey.Formatting (integerToInt, timeFromComponents)
import Data.Time.Calendar
import Data.Time.LocalTime
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.List as List
import Data.Aeson

-- add Maybe monad to some type
-- have videos be a map
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Game
    year Int
    season Season
    gameId Int
    awayId String
    homeId String
    date Day
    time TimeOfDay
    tv String
    state GameState
    period Int
    periodTime String
    awayScore Int
    homeScore Int
    awaySog Int
    homeSog Int
    awayStatus String default=''
    homeStatus String default=''
    awayHighlight String default=''
    homeHighlight String default=''
    awayCondense String default=''
    homeCondense String default=''
    active Bool default=true
    UniqueGameId gameId
    deriving Show
Period
    year Int
    season Season
    gameId Int
    teamId String
    period Int
    shots Int
    goals Int
    UniquePeriodId gameId teamId period
    deriving Show
Event
    eventId Int
    year Int
    season Season
    gameId Int
    teamId String
    period Int
    time String
    eventType EventType
    description String
    videoLink String
    formalId String
    strength Strength
    UniqueEventId eventId gameId
    deriving Show
Team
    teamId String
    city String
    name String
    UniqueTeamId teamId
    deriving Show
PlayoffSeed
    year Int
    conference String
    round Int
    series Int
    homeId String
    awayId String
    homeSeed Int default=0
    awaySeed Int default=0
    UniquePlayoffSeedId year conference round series
    deriving Show Eq
|]

migrate :: (MonadBaseControl IO m, MonadIO m) => Database -> m ()
migrate database = database `process` (runMigration migrateAll)

selectTimeForGame :: (MonadBaseControl IO m, MonadIO m) => Database -> Int -> m TimeOfDay
selectTimeForGame database gameId = do
    games <- database `process` (selectList [GameGameId ==. gameId] [LimitTo 1])
    case games of
        [] -> return $ timeFromComponents 0 0
        (x:xs) -> return $ (gameTime (entityVal x))

selectGames :: (MonadBaseControl IO m, MonadIO m) => Database -> Day -> Day -> m [Game]
selectGames database start end = do
    games <- database `process` selectList [GameDate >=. start, GameDate <=. end] []
    return $ List.map entityVal games

selectPeriods :: (MonadBaseControl IO m, MonadIO m) => Database -> Year -> Season -> m [Period]
selectPeriods database year season =  do
    periods <- database `process` (selectList [PeriodYear ==. (integerToInt (fst year)), PeriodSeason ==. season] [])
    return $ List.map entityVal periods

selectSeeds :: (MonadBaseControl IO m, MonadIO m) => Database -> Year -> m [PlayoffSeed]
selectSeeds database year =  do
    seeds <- database `process` (selectList [PlayoffSeedYear ==. (integerToInt (fst year))] [])
    return $ List.map entityVal seeds

selectGamesForSeason :: (MonadBaseControl IO m, MonadIO m) => Database -> Year -> Season -> m [Game]
selectGamesForSeason database year season =  do
    games <- database `process` (selectList [GameYear ==. (integerToInt (fst year)), GameSeason ==. season] [])
    return $ List.map entityVal games

selectEvents :: (MonadBaseControl IO m, MonadIO m) => Database -> Year -> Season -> m [Event]
selectEvents database year season =  do
    events <- database `process` (selectList ([EventYear ==. (integerToInt (fst year)), EventSeason ==. season] ++ ([EventEventType ==. Goal] ||. [EventEventType ==. Penalty])) [])
    return $ List.map entityVal events

selectGamesForSeries :: (MonadBaseControl IO m, MonadIO m) => Database -> Year -> String -> String -> m [Game]
selectGamesForSeries database year topSeed bottomSeed = do
    games <- database `process` (selectList ([GameYear ==. (integerToInt (fst year)), GameSeason ==. Playoffs, GameHomeId ==. topSeed, GameAwayId ==. bottomSeed] ||. [GameYear ==. (integerToInt (fst year)), GameSeason ==. Playoffs, GameHomeId ==. bottomSeed, GameAwayId ==. topSeed])[])
    return $ List.map entityVal games

updateGamesToInactive :: (MonadBaseControl IO m, MonadIO m) => Database -> [Game] -> m ()
updateGamesToInactive db games = db `process` (updateWhere ([GameGameId <-. (List.map gameGameId games)] ++ ([GameState ==. None] ||. [GameState ==. TBD])) [GameActive =. False])
