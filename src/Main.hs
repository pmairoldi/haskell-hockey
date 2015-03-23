import Network.HTTP
import Formatting
import Data.Text.Lazy as LT
import Data.List as L
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL

currentYear :: Int
currentYear = 2014

data Season = Preseason | Season | Playoffs deriving (Enum, Show, Eq)
data JSONType = JSON | JSONP | HTML deriving (Enum, Show, Eq)

fromSeason :: Season -> Int
fromSeason Preseason = 1
fromSeason Season = 2
fromSeason Playoffs = 3

toSeason :: Int -> Season
toSeason 1 = Preseason
toSeason 2 = Season
toSeason 3 = Playoffs

openUrl :: String -> IO String
openUrl url = simpleHTTP (getRequest url) >>= getResponseBody

get :: String -> JSONType -> IO String
get url responseType
    | responseType == JSONP = do 
        body <- response
        return $ jsonpToJson body
    | otherwise = do 
        body <- response
        return $ body
    where response = openUrl url

dropAndReverse :: Eq a => a -> [a] -> [a]
dropAndReverse delimiter content = L.reverse (L.dropWhile (/= delimiter) content)

jsonpToJson :: String -> String
jsonpToJson jsonp 
    | and $ [("{" `L.isPrefixOf` jsonp), ("}" `L.isSuffixOf` jsonp)] = jsonp
    | otherwise = dropAndReverse '}' (dropAndReverse '{' jsonp)

-- Urls
boxscoreHTMLUrl :: String -> String
boxscoreHTMLUrl id = "http://www.nhl.com/gamecenter/en/boxscore?id=" ++ id

playByPlayUrl :: String -> String -> String
playByPlayUrl year id = "http://live.nhl.com/GameData/" ++ year ++ "/" ++ id ++ "/PlayByPlay.json"

summaryUrl :: String -> String -> String
summaryUrl year id = "http://live.nhl.com/GameData/" ++ year ++ "/" ++ id ++ "/Summary.json"

eventVideoUrl :: String -> String -> String
eventVideoUrl year id = "http://live.nhl.com/GameData/" ++ year ++ "/" ++ id ++ "/gc/gcgm.jsonp"

videoUrl :: String -> String -> String
videoUrl year id = "http://live.nhl.com/GameData/" ++ year ++ "/" ++ id ++ "/gc/gcsb.jsonp"

dateResultsUrl :: String -> String
dateResultsUrl date = "http://live.nhl.com/GameData/GCScoreboard/" ++ date ++ ".jsonp"

boxscoreUrl :: String -> String -> String
boxscoreUrl year id = "http://live.nhl.com/GameData/" ++ year ++ "/" ++ id ++ "/gc/gcbx.jsonp"

gameSummaryUrl :: String -> String -> String
gameSummaryUrl year id = "http://live.nhl.com/GameData/" ++ year ++ "/" ++ id ++ "/gc/gcgs.jsonp"

-- Formatted data
formattedGame :: Int -> String
formattedGame game = LT.unpack (format (left 4 '0') game)

formattedSeason :: Season -> String
formattedSeason season = LT.unpack (format (left 2 '0') (fromSeason season))

formattedYear :: Int -> String
formattedYear year = LT.unpack (format (int) year)

fullGameId :: Int -> Season -> Int -> String
fullGameId year season game = (formattedYear year) ++ (formattedSeason season) ++ (formattedGame game)

fullYear :: Int -> String
fullYear year = (formattedYear year) ++ (formattedYear (year + 1))

-- HTTP requests
getGameSummary :: Int -> Season -> Int -> IO String
getGameSummary year season game = get (gameSummaryUrl (fullYear year) (fullGameId year season game)) JSONP

main :: IO()
main = do src <- getGameSummary 2014 Season 111 
          print src