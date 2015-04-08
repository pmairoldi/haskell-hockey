{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hockey.Parsing (
    decodeResponse
) where

import Hockey.Formatting
import Data.Aeson
import Control.Applicative as Applicative
import Hockey.Types

decodeResponse :: (FromJSON a) => IO String -> IO (Maybe a)
decodeResponse response = do
    rsp <- response
    return $ decode (stringToLazyByteString rsp)

-- GameState
instance FromJSON GameState

-- Game
instance FromJSON Game where
    parseJSON (Object v) = parseGame v
    parseJSON _          = Applicative.empty

parseGame v = Game <$>
    v .: "id" <*>
    fmap unpackToLower (v .: "ata") <*>
    fmap unpackToLower (v .: "hta") <*>
    fmap splitAndJoin (v .: "canationalbroadcasts") <*>
    fmap splitAndJoin (v .: "usnationalbroadcasts") <*>
    fmap toGameState (v .:"gs") <*>
    fmap valueToInteger (v .: "ats") <*>
    fmap valueToInteger (v .: "hts") <*>
    fmap valueToInteger (v .:? "atsog") <*>
    fmap valueToInteger (v .:? "htsog") <*>
    fmap unpackParseTime (v .: "bs") <*>
    fmap removeGameTime (v .: "bs")

-- Results
instance FromJSON Results where
    parseJSON (Object v) = parseResults v
    parseJSON _          = Applicative.empty

parseResults v = Results <$>
    v .: "games" <*>
    fmap unpackParseDate (v .: "currentDate") <*>
    fmap unpackParseDate (v .: "nextDate") <*>
    fmap unpackParseDate (v .: "prevDate")
