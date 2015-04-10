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
    Event(..)
)

where

import Database.Persist.Postgresql hiding (migrate)
import Database.Persist.Sqlite hiding (migrate)
import Database.Persist.TH
import Hockey.Database.Internal
import Hockey.Types (GameState(..), EventType(..))
import Data.Time.Calendar
import Data.Time.LocalTime

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
    strength Int
    UniqueEventId eventId gameId teamId period eventType
    deriving Show
|]

migrate database = database `process` (runMigration migrateAll)
