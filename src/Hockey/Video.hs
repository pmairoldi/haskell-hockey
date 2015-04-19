module Hockey.Video (
    videoUrl
)

where

import Data.Time.Calendar
import Hockey.Formatting

videoUrl :: Day -> Year -> Season -> Int -> String -> String -> HomeAway -> String
videoUrl date seasonYear season game awayAbr homeAbr homeAway =
    let tripleDate = toGregorian date
    in case seasonYear of
        (2014, 2015) -> videoUrl2015 (year tripleDate) (month tripleDate) (day tripleDate) game season awayAbr homeAbr seasonYear homeAway
        otherwise -> []

videoUrl2015 :: Integer -> Integer -> Integer -> Int -> Season -> String -> String -> Year -> HomeAway -> String
videoUrl2015 year month day game season awayAbr homeAbr seasonYear homeAway = "http://smb.cdnak.nyc.neulion.net/u/nhlmobile/vod/nhl/" ++ (formattedYear year) ++ "/" ++ (formattedMonth month) ++ "/" ++ (formattedDay day) ++ "/" ++ (show game) ++ "/" ++ (digitFormat 1 (fromSeason season)) ++ "_" ++ (show game) ++ "_" ++ (stringToLower awayAbr) ++ "_" ++ (stringToLower homeAbr) ++"_" ++ (shortYear seasonYear) ++ "_" ++ (fromHomeAway homeAway) ++ "_continuous_1_1600.mp4"
