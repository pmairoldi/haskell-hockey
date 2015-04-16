module Hockey.Formatting (
    module Hockey.Types,
    module Hockey.Teams,
    module Data.Time.Calendar,
    module Data.Time.LocalTime,
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
    stringToInt,
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
    splitCommaDelimited,
    periodFromPeriodTime,
    valueToInteger,
    splitAndJoin,
    joinStrings,
    gameIdComponents,
    yearFromGameId,
    seasonFromGameId,
    gameFromGameId,
    teamIdFromName,
    cmpTeam,
    months,
    stringToLower,
    seasonYears,
    cmpSeason,
    integerToInt,
    intToInteger
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
import Data.Aeson
import Data.Scientific
import Hockey.Teams

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

stringToLower :: String -> String
stringToLower v = unpackToLower (LazyText.pack v)

-- return 0000-00-00 if not good
dateFromComponents :: Integer -> Int -> Int -> Day
dateFromComponents year month day = fromJust $ fromGregorianValid year month day

stringToInteger :: String -> Integer
stringToInteger [] = 0
stringToInteger s = read s :: Integer

stringToInt :: String -> Int
stringToInt [] = 0
stringToInt s = read s :: Int

integerToInt :: Integer -> Int
integerToInt i = read (show i) :: Int

intToInteger :: Int -> Integer
intToInteger i = read (show i) :: Integer

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

capitalized :: String -> String
capitalized [] = []
capitalized (head:tail) = Char.toUpper head : lowered tail
    where
    lowered [] = []
    lowered (head:tail) = Char.toLower head : lowered tail

removeGameTime :: String -> String
removeGameTime value
        | stringContainsAMPM value = ""
        | otherwise = capitalized (List.takeWhile (/= ' ') value)

-- make function take day instead
year :: (Integer, Int, Int) -> Integer
year (x,_,_) = x

month :: (Integer, Int, Int) -> Integer
month (_,x,_) = toInteger x

day :: (Integer, Int, Int) -> Integer
day (_,_,x) = toInteger x

splitCommaDelimited :: String -> [String]
splitCommaDelimited xs = Split.splitOn "'" xs

periodFromPeriodTime :: String -> Int
periodFromPeriodTime t
    | "1st" `List.isSuffixOf` t = 1
    | "2nd" `List.isSuffixOf` t = 2
    | "3rd" `List.isSuffixOf` t = 3
    | t == "final" = 3
    | "ot" `List.isSuffixOf` t =
        let offset = stringToInt $ List.filter isDigit (List.dropWhile (/= ' ') t)
        in case offset of
            0 -> 4
            otherwise -> 3 + offset
    | "so" `List.isSuffixOf` t = 5
    | otherwise = 0

-- Stupid NHL returning "" in their json when it is a number
valueToInteger :: Maybe Value -> Int
valueToInteger (Just (Number n)) = fromInteger (coefficient n)
valueToInteger _ = 0

splitAndJoin :: String -> String
splitAndJoin s = List.intercalate "," $ (Split.splitOn ", " s)

joinStrings :: String -> String -> String
joinStrings [] [] = ""
joinStrings s1 [] = s1
joinStrings [] s2 = s2
joinStrings s1 s2 = s1 ++ "," ++ s2

gameIdComponents :: Int -> (Int, Season, Int)
gameIdComponents gameId =
    let numbers = (show $ gameId)
    in case List.length numbers of
    10 -> (stringToInt (List.take 4 numbers), toSeason (stringToInteger (List.take 2 (List.drop 4 numbers))), stringToInt (List.take 4 (List.drop 6 numbers)))
    otherwise -> (0, Preseason, 0)

yearFromGameId :: Int -> Int
yearFromGameId g = case (gameIdComponents g) of
    (x,_,_) -> x

seasonFromGameId :: Int -> Season
seasonFromGameId g = case (gameIdComponents g) of
    (_,x,_) -> x

gameFromGameId :: Int -> Int
gameFromGameId  g = case (gameIdComponents g) of
    (_,_,x) -> x

cmpTeam :: String -> Team -> Bool
cmpTeam x y = case y of
    t -> (stringToLower x) == (stringToLower (abr t)) || (stringToLower x) == (stringToLower (city t)) || (stringToLower x) == (stringToLower (name t))

teamFromName :: String -> Maybe Team
teamFromName name = case List.filter (\x -> cmpTeam name x) teams of
    [x] -> Just x
    otherwise -> Nothing

teamIdFromName :: String -> String
teamIdFromName name = case teamFromName name of
    Just x -> (abr x)
    Nothing -> ""

preseasonMonths :: Year -> [Year]
preseasonMonths year = [((fst year), 9), ((fst year), 10)]

seasonMonths :: Year -> [Year]
seasonMonths year = [((fst year), 10), ((fst year), 11), ((fst year), 12), ((snd year), 1), ((snd year), 2), ((snd year), 3), ((snd year), 4)]

playoffMonths :: Year -> [Year]
playoffMonths year = [((snd year), 4), ((snd year), 5), ((snd year), 6)]

months :: Season -> Year -> [Year]
months Preseason years= preseasonMonths years
months Season years = seasonMonths years
months Playoffs years = playoffMonths years

seasonYears :: Integer -> Year
seasonYears year = (year, year + 1)

cmpSeason :: GameDate -> Season -> Bool
cmpSeason x s = (season x) == s
