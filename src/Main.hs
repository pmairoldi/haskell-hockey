{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable#-}

import Network.HTTP
import Formatting
import Data.Text.Lazy as LT
import Data.List as L
import Data.Aeson
-- import Data.Typeable
import GHC.Generics
-- import Data.Data
-- import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C

-- Load from environment variables
currentYear :: Int
currentYear = 2014

currentSeason :: Season
currentSeason = Playoffs

data JSONType = JSON | JSONP | HTML deriving (Enum, Show, Eq)

data Season = Preseason | Season | Playoffs deriving (Enum, Show, Eq)

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

jsonpRequestParse :: IO String -> IO String
jsonpRequestParse response = do 
    body <- response
    return $ jsonpToJson body

requestParse :: IO String -> IO String
requestParse response = do 
    body <- response
    return $ body

get :: String -> JSONType -> IO String
get url responseType
    | responseType == JSONP = jsonpRequestParse response
    | otherwise = requestParse response
    where response = openUrl url

dropAndReverse :: Eq a => a -> [a] -> [a]
dropAndReverse delimiter content = L.reverse (L.dropWhile (/= delimiter) content)

jsonpToJson :: String -> String
jsonpToJson jsonp 
    | and $ [("{" `L.isPrefixOf` jsonp), ("}" `L.isSuffixOf` jsonp)] = jsonp
    | otherwise = dropAndReverse '}' (dropAndReverse '{' jsonp)

-- Urls
playByPlayUrl :: Int -> Season -> Int -> String
playByPlayUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/PlayByPlay.json"

summaryUrl :: Int -> Season -> Int -> String
summaryUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/Summary.json"

eventVideoUrl :: Int -> Season -> Int -> String
eventVideoUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcgm.jsonp"

videoUrl :: Int -> Season -> Int -> String
videoUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcsb.jsonp"

boxscoreUrl :: Int -> Season -> Int -> String
boxscoreUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcbx.jsonp"

gameSummaryUrl :: Int -> Season -> Int -> String
gameSummaryUrl year season game = "http://live.nhl.com/GameData/" ++ (fullYear year) ++ "/" ++ (fullGameId year season game) ++ "/gc/gcgs.jsonp"

resultsUrl :: Int -> Int -> Int -> String
resultsUrl year month day = "http://live.nhl.com/GameData/GCScoreboard/" ++ (fullDate year month day) ++ ".jsonp"

boxscoreHTMLUrl :: Int -> Season -> Int -> String
boxscoreHTMLUrl year season game = "http://www.nhl.com/gamecenter/en/boxscore?id=" ++ (fullGameId year season game)

-- Formatters
digitFormat :: Int -> Int -> String 
digitFormat digits number = LT.unpack (format (left digits '0') number)

formattedGame :: Int -> String
formattedGame game = digitFormat 4 game

formattedSeason :: Season -> String
formattedSeason season = digitFormat 2 (fromSeason season)

formattedMonth :: Int -> String
formattedMonth month = digitFormat 2 month

formattedDay :: Int -> String
formattedDay day = digitFormat 2 day

formattedYear :: Int -> String
formattedYear year = LT.unpack (format (int) year)

fullGameId :: Int -> Season -> Int -> String
fullGameId year season game = (formattedYear year) ++ (formattedSeason season) ++ (formattedGame game)

fullYear :: Int -> String
fullYear year = (formattedYear year) ++ (formattedYear (year + 1))

fullDate :: Int -> Int -> Int -> String
fullDate year month day = (formattedYear year) ++ "-" ++ (formattedMonth month) ++ "-" ++ (formattedDay day)

-- HTTP requests
getPlayByPlayResponse :: Int -> Season -> Int -> IO String
getPlayByPlayResponse year season game = get (playByPlayUrl year season game) JSON

getSummaryResponse :: Int -> Season -> Int -> IO String
getSummaryResponse year season game = get (summaryUrl year season game) JSON

getEventVideosResponse :: Int -> Season -> Int -> IO String
getEventVideosResponse year season game = get (eventVideoUrl year season game) JSONP

getVideosResponse :: Int -> Season -> Int -> IO String
getVideosResponse year season game = get (videoUrl year season game) JSONP

getBoxscoreResponse :: Int -> Season -> Int -> IO String
getBoxscoreResponse year season game = get (boxscoreUrl year season game) JSONP

getGameSummaryResponse :: Int -> Season -> Int -> IO String
getGameSummaryResponse year season game = get (gameSummaryUrl year season game) JSONP

getResultsResponse :: Int -> Int -> Int -> IO String
getResultsResponse year month day = get (resultsUrl year month day) JSONP

-- Parse JSON

data Game = Game { ata :: String, hta :: String } deriving (Show, Generic)
instance FromJSON Game
instance ToJSON Game

data Results = Results { games :: [Game] } deriving (Show, Generic)
instance FromJSON Results
instance ToJSON Results

stringToLazyByteString :: String -> LBS.ByteString
stringToLazyByteString string = LBS.fromStrict (C.pack string)

decodeResponse :: (FromJSON a) => IO String -> IO (Maybe a)
decodeResponse response = do 
    rsp <- response
    return $ decode (stringToLazyByteString rsp)

-- Start add concrete types

getPlayByPlay :: (FromJSON a) => Int -> Season -> Int -> IO (Maybe a)
getPlayByPlay year season game = decodeResponse $ getPlayByPlayResponse year season game

getSummary :: (FromJSON a) => Int -> Season -> Int -> IO (Maybe a)
getSummary year season game = decodeResponse $ getSummaryResponse year season game

getEventVideos :: (FromJSON a) => Int -> Season -> Int -> IO (Maybe a)
getEventVideos year season game = decodeResponse $ getEventVideosResponse year season game

getVideos :: (FromJSON a) => Int -> Season -> Int -> IO (Maybe a)
getVideos year season game = decodeResponse $ getVideosResponse year season game

getBoxscore :: (FromJSON a) => Int -> Season -> Int -> IO (Maybe a)
getBoxscore year season game = decodeResponse $ getBoxscoreResponse year season game

getGameSummary :: (FromJSON a) => Int -> Season -> Int -> IO (Maybe a)
getGameSummary year season game = decodeResponse $ getGameSummaryResponse year season game

-- End add concrete types

getResults :: Int -> Int -> Int -> IO (Maybe Results)
getResults year month day = decodeResponse $ getResultsResponse year month day

main = print "hello world"
-- main :: IO()
-- main = do 
--     src <- getGameSummaryResponse 2014 Season 111 
--     print src
