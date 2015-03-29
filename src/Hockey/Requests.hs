module Hockey.Requests (
    -- getPlayByPlay,
    -- getSummary,
    -- getEventVideos,
    -- getVideos,
    -- getBoxscore,
    -- getGameSummary,
    getResults,
) where

import Hockey.Network
import Hockey.Parsing
import Hockey.Types
import Hockey.Formatting
import Data.UTC

-- URLs

playByPlayUrl :: Integer -> Season -> Integer -> (String, ReturnType)
playByPlayUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/PlayByPlay.json", JSON)

summaryUrl :: Integer -> Season -> Integer -> (String, ReturnType)
summaryUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/Summary.json", JSON)

eventVideoUrl :: Integer -> Season -> Integer -> (String, ReturnType)
eventVideoUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcgm.jsonp", JSONP)

videoUrl :: Integer -> Season -> Integer -> (String, ReturnType)
videoUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcsb.jsonp", JSONP)

boxscoreUrl :: Integer -> Season -> Integer -> (String, ReturnType)
boxscoreUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcbx.jsonp", JSONP)

gameSummaryUrl :: Integer -> Season -> Integer -> (String, ReturnType)
gameSummaryUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcgs.jsonp", JSONP)

resultsUrl :: Integer -> Integer -> Integer -> (String, ReturnType)
resultsUrl year month day = ("http://live.nhl.com/GameData/GCScoreboard/" ++ (fullDate year month day) ++ ".jsonp", JSONP)

boxscoreHTMLUrl :: Integer -> Season -> Integer -> (String, ReturnType)
boxscoreHTMLUrl year season game = ("http://www.nhl.com/gamecenter/en/boxscore?id=" ++ (fullGameId year season game), HTML)

-- HTTP Requests

getResponse :: (String, ReturnType) -> IO String
getResponse tuple = get (fst tuple) (snd tuple)

-- Parsed Responses

-- Start add concrete types

-- getPlayByPlay :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getPlayByPlay year season game = decodeResponse $ getResponse $ playByPlayUrl year season game
--
-- getSummary :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getSummary year season game = decodeResponse $ getResponse $ summaryUrl year season game
--
-- getEventVideos :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getEventVideos year season game = decodeResponse $ getResponse $ eventVideoUrl year season game
--
-- getVideos :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getVideos year season game = decodeResponse $ getResponse $ videoUrl year season game
--
-- getBoxscore :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getBoxscore year season game = decodeResponse $ getResponse $ boxscoreUrl year season game
--
-- getGameSummary :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getGameSummary year season game = decodeResponse $ getResponse $ gameSummaryUrl year season game

-- End add concrete types

getResults :: Date -> IO (Maybe Results)
getResults date = decodeResponse $ getResponse $ resultsUrl (year date) (month date) (day date)
