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
    stringToLazyByteString
) where

import Hockey.Types
import Formatting
import Data.Text.Lazy as LazyText
import Data.UTC
import Data.Maybe
import Data.List as List
import Data.List.Split as Split
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as ByteString

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

stringToLazyByteString :: String -> LazyByteString.ByteString
stringToLazyByteString string = LazyByteString.fromStrict (ByteString.pack string)
