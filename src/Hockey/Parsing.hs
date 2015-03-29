{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Hockey.Parsing (
    decodeResponse,
) where

import Hockey.Formatting
import Data.Aeson
import Control.Applicative as Applicative
import Hockey.Types
import Data.Scientific

decodeResponse :: (FromJSON a) => IO String -> IO (Maybe a)
decodeResponse response = do
    rsp <- response
    return $ decode (stringToLazyByteString rsp)

-- Stupid NHL returning "" in their json when it is a number
valueToInteger :: Maybe Value -> Integer
valueToInteger (Just (Number n)) = coefficient n
valueToInteger _ = 0

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
    v .: "canationalbroadcasts" <*>
    v .: "usnationalbroadcasts" <*>
    fmap toGameState (v .:"gs") <*>
    fmap valueToInteger (v .: "ats") <*>
    fmap valueToInteger (v .: "hts") <*>
    fmap valueToInteger (v .:? "atsog") <*>
    fmap valueToInteger (v .:? "htsog")

-- Results
instance FromJSON Results where
    parseJSON (Object v) = parseResults v
    parseJSON _          = Applicative.empty

parseResults v = Results <$>
    v .: "games" <*>
    fmap unpackParseDate (v .: "currentDate") <*>
    fmap unpackParseDate (v .: "nextDate") <*>
    fmap unpackParseDate (v .: "prevDate")
