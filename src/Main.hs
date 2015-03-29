{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Network.HTTP
import Formatting
import Data.Text.Lazy as LazyText
import Data.List as List
import Data.Aeson
import Control.Applicative as Applicative
import GHC.Generics
import Data.UTC
import Data.Maybe
import Data.String
import Data.List.Split as Split

-- import Data.Typeable
-- import Data.Data
-- import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as ByteString

-- Load from environment variables
currentYear :: Integer
currentYear = 2014

currentSeason :: Season
currentSeason = Playoffs

data JSONType = JSON | JSONP | HTML deriving (Enum, Show, Eq)

data Season = Preseason | Season | Playoffs deriving (Enum, Show, Eq)

fromSeason :: Season -> Integer
fromSeason Preseason = 1
fromSeason Season = 2
fromSeason Playoffs = 3

toSeason :: Integer -> Season
toSeason 1 = Preseason
toSeason 2 = Season
toSeason 3 = Playoffs

-- Formatters
digitFormat :: Int -> Integer -> String
digitFormat digits number = LazyText.unpack (format (left digits '0') number)

formattedGame :: Integer -> String
formattedGame game = digitFormat 4 game

formattedSeason :: Season -> String
formattedSeason season = digitFormat 2 (fromSeason season)

formattedMonth :: Integer -> String
formattedMonth month = digitFormat 2 month

formattedDay :: Integer -> String
formattedDay day = digitFormat 2 day

formattedYear :: Integer -> String
formattedYear year = LazyText.unpack (format (int) year)

fullGameId :: Integer -> Season -> Integer -> String
fullGameId year season game = (formattedYear year) ++ (formattedSeason season) ++ (formattedGame game)

fullYear :: Integer -> String
fullYear year = (formattedYear year) ++ (formattedYear (year + 1))

fullDate :: Integer -> Integer -> Integer -> String
fullDate year month day = (formattedYear year) ++ "-" ++ (formattedMonth month) ++ "-" ++ (formattedDay day)

unpackToLower :: Text -> String
unpackToLower v = LazyText.unpack (LazyText.toLower v)

dateFromComponents :: Integer -> Integer -> Integer -> Date
dateFromComponents year month day = fromJust $ setYear year (epoch :: Date) >>= setMonth month >>= setDay day

stringToInteger :: String -> Integer
stringToInteger s = read s :: Integer

dateStringToComponents :: Text -> [Integer]
dateStringToComponents text = List.map stringToInteger $ Split.splitOn "/" $ LazyText.unpack text

unpackParseDate :: Text -> Date
unpackParseDate text =
    let components = dateStringToComponents text
    in dateFromComponents (components !! 2) (components !! 0)  (components !! 1)

-- Request helpers
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
dropAndReverse delimiter content = List.reverse (List.dropWhile (/= delimiter) content)

jsonpToJson :: String -> String
jsonpToJson jsonp
    | and $ [("{" `List.isPrefixOf` jsonp), ("}" `List.isSuffixOf` jsonp)] = jsonp
    | otherwise = dropAndReverse '}' (dropAndReverse '{' jsonp)

-- Urls
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

-- HTTP requests
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

-- Parse JSON

data GameState = None | Before | Ongoing | Overtime | Final deriving (Enum, Show, Eq, Generic)

fromGameState :: GameState -> Integer
fromGameState None = 1
fromGameState Before = 2
fromGameState Ongoing = 3
fromGameState Overtime = 4
fromGameState Final = 4

toGameState :: Integer -> GameState
toGameState 1 = None
toGameState 2 = Before
toGameState 3 = Ongoing
toGameState 4 = Overtime
toGameState 5 = Final

instance FromJSON GameState

data Game = Game { id :: Integer, awayId :: String, homeId :: String, caTV :: String, usTV :: String, gameState :: GameState } deriving (Show, Generic)

parseGame v = Game <$>
    v .: "id" <*>
    fmap unpackToLower (v .: "ata") <*>
    fmap unpackToLower (v .: "hta") <*>
    v .: "canationalbroadcasts" <*>
    v .: "usnationalbroadcasts" <*>
    fmap toGameState (v .:"gs")

instance FromJSON Game where
    parseJSON (Object v) = parseGame v
    parseJSON _          = Applicative.empty

data Results = Results { games :: [Game], currentDate :: Date, nextDate :: Date, prevDate :: Date } deriving (Show, Generic)

parseResults v = Results <$>
    v .: "games" <*>
    fmap unpackParseDate (v .: "currentDate") <*>
    fmap unpackParseDate (v .: "nextDate") <*>
    fmap unpackParseDate (v .: "prevDate")

instance FromJSON Results where
    parseJSON (Object v) = parseResults v
    parseJSON _          = Applicative.empty

stringToLazyByteString :: String -> LazyByteString.ByteString
stringToLazyByteString string = LazyByteString.fromStrict (ByteString.pack string)

decodeResponse :: (FromJSON a) => IO String -> IO (Maybe a)
decodeResponse response = do
    rsp <- response
    return $ decode (stringToLazyByteString rsp)

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

main = print "hello world"
-- main :: IO()
-- main = do
--     src <- getGameSummaryResponse 2014 Season 111
--     prInteger src
