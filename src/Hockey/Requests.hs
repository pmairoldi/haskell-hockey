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
    getResults,
    getGameDates,
    getGameEvents,
    getGamePeriods,
    getVideo
) where

import Hockey.Network
import Hockey.Parsing
import Hockey.Types
import Hockey.Formatting
import Data.Time.Calendar
import Hockey.Video

-- add type alias to make it easier to understand

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
resultsUrl year month day = ("http://statsapi.web.nhl.com/api/v1/schedule?startDate=" ++ (fullDate year month day) ++ "&endDate=" ++ (fullDate year month day) ++ "&expand=schedule.teams,schedule.linescore,schedule.broadcasts.all,schedule.scoringplays,schedule.game.seriesSummary,seriesSummary.series", JSON)

-- year is season start year
boxscoreHTMLUrl :: Integer -> Season -> Integer -> (String, ReturnType)
boxscoreHTMLUrl year season game = ("http://www.nhl.com/gamecenter/en/boxscore?id=" ++ (fullGameId year season game), HTML)

-- year is date year
calendarUrl :: Integer -> Integer -> (String, ReturnType)
calendarUrl year month = ("http://statsapi.web.nhl.com/api/v1/schedule?startDate=" ++ (fullDate year month 1) ++ "&endDate=" ++ (fullDate year month (lastDay year month)), JSON)

-- HTTP Requests
getResponse :: (String, ReturnType) -> IO String
getResponse tuple = get (fst tuple) (snd tuple)

-- Parsed Responses
getResults :: Day -> IO (Maybe Results)
getResults date =
    let tripleDate = toGregorian date
    in decodeResponse $ getResponse $ resultsUrl (year tripleDate) (month tripleDate) (day tripleDate)

getGameDates :: (Integer, Integer) -> IO (Maybe Results)
getGameDates (x, y) = decodeResponse $ getResponse $ calendarUrl x y

getGameEvents :: Integer -> Season -> Integer -> IO (Maybe GameEvents)
getGameEvents year season game = decodeResponse $ getResponse $ playByPlayUrl year season game

getGamePeriods :: Integer -> Season -> Integer -> IO (Maybe Scoreboard)
getGamePeriods year season game = decodeResponse $ getResponse $ scoreboardUrl year season game

getVideo :: Day -> Year -> Season -> Int -> String -> String -> HomeAway -> IO (Maybe String)
getVideo date year season game awayAbr homeAbr homeAway = ping $ videoUrl date year season (gameFromGameId game) awayAbr homeAbr homeAway
