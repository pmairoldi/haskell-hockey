module Hockey.Requests (
    getPlayByPlay,
    getSummary,
    getEventVideos,
    getVideos,
    getBoxscore,
    getGameSummary,
    getResults
) where

import Hockey.Network
import Hockey.Parsing
import Hockey.Types
import Hockey.Formatting
import Data.UTC

-- URLs

playByPlayUrl :: Integer -> Season -> Integer -> String
playByPlayUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/PlayByPlay.json"

summaryUrl :: Integer -> Season -> Integer -> String
summaryUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/Summary.json"

eventVideoUrl :: Integer -> Season -> Integer -> String
eventVideoUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcgm.jsonp"

videoUrl :: Integer -> Season -> Integer -> String
videoUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcsb.jsonp"

boxscoreUrl :: Integer -> Season -> Integer -> String
boxscoreUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcbx.jsonp"

gameSummaryUrl :: Integer -> Season -> Integer -> String
gameSummaryUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcgs.jsonp"

resultsUrl :: Integer -> Integer -> Integer -> String
resultsUrl year month day = "http://live.nhl.com/GameData/GCScoreboard/" ++ (fullDate year month day) ++ ".jsonp"

boxscoreHTMLUrl :: Integer -> Season -> Integer -> String
boxscoreHTMLUrl year season game = "http://www.nhl.com/gamecenter/en/boxscore?id=" ++ (fullGameId year season game)

-- HTTP Requests

getPlayByPlayResponse :: Integer -> Season -> Integer -> IO String
getPlayByPlayResponse year season game = get (playByPlayUrl year season game) JSON

getSummaryResponse :: Integer -> Season -> Integer -> IO String
getSummaryResponse year season game = get (summaryUrl year season game) JSON

getEventVideosResponse :: Integer -> Season -> Integer -> IO String
getEventVideosResponse year season game = get (eventVideoUrl year season game) JSONP

getVideosResponse :: Integer -> Season -> Integer -> IO String
getVideosResponse year season game = get (videoUrl year season game) JSONP

getBoxscoreResponse :: Integer -> Season -> Integer -> IO String
getBoxscoreResponse year season game = get (boxscoreUrl year season game) JSONP

getGameSummaryResponse :: Integer -> Season -> Integer -> IO String
getGameSummaryResponse year season game = get (gameSummaryUrl year season game) JSONP

getResultsResponse :: Integer -> Integer -> Integer -> IO String
getResultsResponse year month day = get (resultsUrl year month day) JSONP

-- Parsed Responses

-- Start add concrete types

getPlayByPlay :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
getPlayByPlay year season game = decodeResponse $ getPlayByPlayResponse year season game

getSummary :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
getSummary year season game = decodeResponse $ getSummaryResponse year season game

getEventVideos :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
getEventVideos year season game = decodeResponse $ getEventVideosResponse year season game

getVideos :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
getVideos year season game = decodeResponse $ getVideosResponse year season game

getBoxscore :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
getBoxscore year season game = decodeResponse $ getBoxscoreResponse year season game

getGameSummary :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
getGameSummary year season game = decodeResponse $ getGameSummaryResponse year season game

-- End add concrete types

getResults :: Date -> IO (Maybe Results)
getResults date = decodeResponse $ getResultsResponse (year date) (month date) (day date)
