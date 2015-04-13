{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hockey.Database.Types (
    migrate,
    Game(..),
    Video(..),
    Event(..),
    Team(..),
    PlayoffSeed(..),
    Period(..),
    selectGames
)

where

import Database.Persist.Postgresql hiding (migrate)
import Database.Persist.Sqlite hiding (migrate)
import Database.Persist.TH
import Hockey.Database.Internal
import Hockey.Types (GameState(..), EventType(..), Strength(..))
import Data.Time.Calendar
import Data.Time.LocalTime
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.List as List

--add Maybe monad to some type
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Game
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
    awayStatus String
    homeStatus String
    UniqueGameId gameId
    deriving Show
Period
    gameId Int
    teamId String
    period Int
    shots Int
    goals Int
    UniquePeriodId gameId teamId period
    deriving Show
Video
    gameId Int
    awayId String
    homeId String
    awayHighlight String
    homeHighlight String
    awayCondense String
    homeCondense String
    UniqueVideoId gameId
    deriving Show
Event
    eventId Int
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
    seed Int
    homeId String
    awayId String
    UniquePlayoffSeedId year conference round seed
    deriving Show
|]

migrate :: (MonadBaseControl IO m, MonadIO m) => Database -> m ()
migrate database = database `process` (runMigration migrateAll)

selectGames :: (MonadBaseControl IO m, MonadIO m) => Database -> [Day] -> m [Game]
selectGames database dates = do
    games <- database `process` (selectList [GameDate <-. dates] [])
    return $ List.map entityVal games
