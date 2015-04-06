module Hockey.Requests (
    playByPlayUrl,
    summaryUrl,
    eventVideoUrl,
    scoreboardUrl,
    boxscoreUrl,
    gameSummaryUrl,
    resultsUrl,
    boxscoreHTMLUrl,
    calendarUrl,
    getResponse,
    -- getPlayByPlay,
    -- getSummary,
    -- getEventVideos,
    -- getScoreboard,
    -- getBoxscore,
    -- getGameSummary,
    getResults
) where

import Hockey.Network
import Hockey.Parsing
import Hockey.Types
import Hockey.Formatting
import Data.Time.Calendar

-- URLs

-- year is season start year
playByPlayUrl :: Integer -> Season -> Integer -> (String, ReturnType)
playByPlayUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/PlayByPlay.json", JSON)

-- year is season start year
summaryUrl :: Integer -> Season -> Integer -> (String, ReturnType)
summaryUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/Summary.json", JSON)

-- year is season start year
eventVideoUrl :: Integer -> Season -> Integer -> (String, ReturnType)
eventVideoUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcgm.jsonp", JSONP)

-- year is season start year
scoreboardUrl :: Integer -> Season -> Integer -> (String, ReturnType)
scoreboardUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcsb.jsonp", JSONP)

-- year is season start year
boxscoreUrl :: Integer -> Season -> Integer -> (String, ReturnType)
boxscoreUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcbx.jsonp", JSONP)

-- not really useful
-- year is season start year
gameSummaryUrl :: Integer -> Season -> Integer -> (String, ReturnType)
gameSummaryUrl year season game = ("http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcgs.jsonp", JSONP)

-- year is date year
resultsUrl :: Integer -> Integer -> Integer -> (String, ReturnType)
resultsUrl year month day = ("http://live.nhl.com/GameData/GCScoreboard/" ++ (fullDate year month day) ++ ".jsonp", JSONP)

-- year is season start year
boxscoreHTMLUrl :: Integer -> Season -> Integer -> (String, ReturnType)
boxscoreHTMLUrl year season game = ("http://www.nhl.com/gamecenter/en/boxscore?id=" ++ (fullGameId year season game), HTML)

-- year is date year
calendarUrl :: Integer -> Integer -> (String, ReturnType)
calendarUrl year month = ("http://www.nhl.com/gamecenter/en/ajax/gamecalendarjson?year=" ++ (formattedYear year) ++ "&month=" ++ (formattedMonth month), JSON)
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
-- getScoreboard :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getScoreboard year season game = decodeResponse $ getResponse $ scoreboardUrl year season game
--
-- getBoxscore :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getBoxscore year season game = decodeResponse $ getResponse $ boxscoreUrl year season game
--
-- getGameSummary :: (FromJSON a) => Integer -> Season -> Integer -> IO (Maybe a)
-- getGameSummary year season game = decodeResponse $ getResponse $ gameSummaryUrl year season game

-- End add concrete types

getResults :: Day -> IO (Maybe Results)
getResults date =
    let tripleDate = toGregorian date
    in decodeResponse $ getResponse $ resultsUrl (year tripleDate) (month tripleDate) (day tripleDate)
