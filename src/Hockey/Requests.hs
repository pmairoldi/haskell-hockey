module Hockey.Requests (
    getResults,
    getGameDates,
    getStandings,
    getGameEvents,
    getVideo
) where

import Hockey.Network
import Hockey.Parsing
import Hockey.Types
import Hockey.Types.Standings (Standings(..))
import Hockey.Types.Events (Events(..))
import Hockey.Formatting
import Data.Time.Calendar
import Hockey.Video

-- add type alias to make it easier to understand

-- URLs

-- year is date year
resultsUrl :: Day -> Day -> (String, ReturnType)
resultsUrl from to = ("http://statsapi.web.nhl.com/api/v1/schedule?startDate=" ++ showGregorian from ++ "&endDate=" ++ showGregorian to ++ "&expand=schedule.teams,schedule.linescore,schedule.broadcasts.all,schedule.scoringplays,schedule.game.seriesSummary,seriesSummary.series,schedule.game.content.media.epg", JSON)

-- year is date year
calendarUrl :: Integer -> Integer -> (String, ReturnType)
calendarUrl year month = ("http://statsapi.web.nhl.com/api/v1/schedule?startDate=" ++ fullDate year month 1 ++ "&endDate=" ++ fullDate year month (lastDay year month) ++ "&expand=schedule.teams,schedule.linescore,schedule.broadcasts.all,schedule.scoringplays,schedule.game.seriesSummary,seriesSummary.series,schedule.game.content.media.epg", JSON)

-- year is season start year + season end year
standingsUrl :: Year -> (String, ReturnType)
standingsUrl year = ("http://statsapi.web.nhl.com/api/v1/standings/wildCardWithLeaders?expand=standings.team&season=" ++ yearToString year, JSON)

eventsUrl :: Integer -> Season -> Integer -> (String, ReturnType)
eventsUrl year season game = ("http://statsapi.web.nhl.com/api/v1/game/" ++ fullGameId year season game ++ "/feed/live", JSON)

-- HTTP Requests
getResponse :: (String, ReturnType) -> IO String
getResponse = uncurry get

-- Parsed Responses
getResults :: Day -> Day -> IO (Maybe Results)
getResults from to = decodeResponse $ getResponse $ resultsUrl from to

getGameDates :: (Integer, Integer) -> IO (Maybe Results)
getGameDates (x, y) = decodeResponse $ getResponse $ calendarUrl x y

getStandings :: Year -> IO (Maybe Standings)
getStandings year = decodeResponse $ getResponse $ standingsUrl year

getGameEvents :: Integer -> Season -> Integer -> IO (Maybe Events)
getGameEvents year season game = decodeResponse $ getResponse $ eventsUrl year season game

getVideo :: Day -> Year -> Season -> Int -> String -> String -> HomeAway -> IO (Maybe String)
getVideo date year season game awayAbr homeAbr homeAway = ping $ videoUrl date year season (gameFromGameId game) awayAbr homeAbr homeAway
