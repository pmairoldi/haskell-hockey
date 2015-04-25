{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Hockey.Database hiding (port)
import Hockey.Environment
import Hockey.Formatting (formattedGame, formattedSeason, formattedYear, intToInteger, fromStrength, fromEventType, boolToInt)
import Control.Monad.IO.Class
import Yesod

-- Period
instance ToJSON Period where
    toJSON Period {..} = object [ "teamID" .= periodTeamId, "gameID" .= periodGameId, "period" .= periodPeriod, "goals" .= periodGoals, "shots" .= periodShots ]

-- Team
instance ToJSON PlayoffSeed where
    toJSON PlayoffSeed {..} = object [ "seasonID" .= ((formattedYear (intToInteger playoffSeedYear)) ++ (formattedSeason playoffSeedSeason)), "conference" .= playoffSeedConference, "seed" .= playoffSeedSeed, "homeID" .= playoffSeedHomeId, "awayID" .= playoffSeedAwayId, "round" .= playoffSeedRound ]

-- Team
instance ToJSON Game where
    toJSON Game {..} = object [ "seasonID" .= ((formattedYear (intToInteger gameYear)) ++ (formattedSeason gameSeason)), "awayID" .= gameAwayId, "homeID" .= gameHomeId, "awayScore" .= gameAwayScore, "homeScore" .= gameHomeScore, "gameID" .= gameGameId, "date" .= (show gameDate), "time" .= (show gameTime), "tv" .= gameTv, "period" .= gamePeriod, "periodTime" .= gamePeriodTime, "homeStatus" .= gameHomeStatus, "awayStatus" .= gameAwayStatus, "homeHighlight" .= gameHomeHighlight, "awayHighlight" .= gameAwayHighlight, "homeCondense" .= gameHomeCondense, "awayCondense" .= gameAwayCondense, "active" .= (boolToInt gameActive)]

-- Event
instance ToJSON Event where
    toJSON Event {..} = object [ "eventID" .= eventEventId, "gameID" .= eventGameId, "teamID" .= eventTeamId, "period" .= eventPeriod, "time" .= eventTime, "type" .= (fromEventType eventEventType), "description" .= eventDescription, "videoLink" .= eventVideoLink, "formalID" .= eventFormalId, "strength" .= (fromStrength eventStrength) ]

data Playoffs = Playoffs {
    periods :: [Period],
    seeds :: [PlayoffSeed],
    games :: [Game],
    events :: [Event]
} deriving Show

instance ToJSON Playoffs where
    toJSON Playoffs {..} = object [ "periods" .= periods, "teams" .= seeds, "games" .= games, "events" .= events ]

data App = App

mkYesod "App" [parseRoutes|
/Hockey/Playoffs PlayoffsR GET
|]

instance Yesod App

getPlayoffsR :: Handler Value
getPlayoffsR = liftIO $ do
    e <- env
    p <- selectPeriods (database e) (year e) (season e)
    s <- selectSeeds (database e) (year e) (season e)
    g <- selectGamesForSeason (database e) (year e) (season e)
    e <- selectEvents (database e) (year e) (season e)

    returnJson $ Playoffs p s g e

main :: IO ()
main = do
    e <- env
    warp (port e) App
