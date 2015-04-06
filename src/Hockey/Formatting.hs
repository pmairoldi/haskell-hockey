module Hockey.Formatting (
    digitFormat,
    formattedGame,
    formattedSeason,
    formattedMonth,
    formattedDay,
    formattedYear,
    fullGameId,
    fullYear,
    fullDate,
    unpackToLower,
    dateFromComponents,
    stringToInteger,
    dateStringToComponents,
    unpackParseDate,
    stringToLazyByteString,
    offsetAMPMHour,
    timeFromComponents,
    timeStringToComponents,
    parseAMPM,
    stringContainsAMPM,
    textContainsAMPM,
    unpackParseTime,
    removeGameTime,
    year,
    month,
    day,
    splitCommaDelimited
) where

import Hockey.Types
import Formatting
import Data.Text.Lazy as LazyText
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Maybe
import Data.List as List
import Data.List.Split as Split
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as ByteString
import Data.Char as Char

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

-- return 0000-00-00 if not good
dateFromComponents :: Integer -> Int -> Int -> Day
dateFromComponents year month day = fromJust $ fromGregorianValid year month day

stringToInteger :: String -> Integer
stringToInteger [] = 0
stringToInteger s = read s :: Integer

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt s = read s :: Int

dateStringToComponents :: Text -> [Int]
dateStringToComponents text = List.map stringToInt $ Split.splitOn "/" $ LazyText.unpack text

unpackParseDate :: Text -> Day
unpackParseDate text =
    let components = dateStringToComponents text
    in dateFromComponents (toInteger (components !! 2)) (components !! 0)  (components !! 1)

stringToLazyByteString :: String -> LazyByteString.ByteString
stringToLazyByteString string = LazyByteString.fromStrict (ByteString.pack string)

offsetAMPMHour :: Int -> AMPM -> Int
offsetAMPMHour hour ampm
    | and $ [hour == 12, ampm == AM] = 0
    | and $ [hour == 12, ampm == PM] = 12
    | ampm == PM = hour + 12
    | otherwise = hour

-- return 00:00 if not good
timeFromComponents :: Int -> Int -> AMPM -> TimeOfDay
timeFromComponents hour minute ampm =
    let offsetHour = offsetAMPMHour hour ampm
    in fromJust $ makeTimeOfDayValid offsetHour minute 0

timeStringToComponents :: Text -> [Int]
timeStringToComponents text = List.map stringToInt $ Split.splitOn ":" $ LazyText.unpack $ LazyText.takeWhile (/= ' ') text

parseAMPM :: Text -> AMPM
parseAMPM text
    | ((LazyText.pack "AM") `LazyText.isSuffixOf` text) = AM
    | ((LazyText.pack "PM") `LazyText.isSuffixOf` text) = PM
    | otherwise = AM

stringContainsAMPM :: String -> Bool
stringContainsAMPM text = or $ [("AM" `List.isSuffixOf` text), ("PM" `List.isSuffixOf` text)]

textContainsAMPM :: Text -> Bool
textContainsAMPM text = or $ [((LazyText.pack "AM") `LazyText.isSuffixOf` text), ((LazyText.pack "PM") `LazyText.isSuffixOf` text)]

unpackParseTime :: Text -> TimeOfDay
unpackParseTime text
    | textContainsAMPM text =
    let components = timeStringToComponents text
        ampm = parseAMPM text
    in timeFromComponents (components !! 0)  (components !! 1) ampm
    | otherwise = timeFromComponents 0 0 AM

removeGameTime :: String -> String
removeGameTime value
        | stringContainsAMPM value = ""
        | otherwise = List.map Char.toLower value

year :: (Integer, Int, Int) -> Integer
year (x,_,_) = x

month :: (Integer, Int, Int) -> Integer
month (_,x,_) = toInteger x

day :: (Integer, Int, Int) -> Integer
day (_,_,x) = toInteger x

splitCommaDelimited :: String -> [String]
splitCommaDelimited xs = Split.splitOn "'" xs
