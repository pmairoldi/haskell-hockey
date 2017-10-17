{-# LANGUAGE OverloadedStrings #-}

module Models.Path
  ( Year(..)
  ) where

import Control.Arrow
import Data.Text
import Text.Read
import Yesod

data Year = Year
  { start :: Integer
  , end :: Integer
  } deriving (Show, Read, Eq)

instance PathPiece Year where
  toPathPiece year = pack (show (start year) ++ show (end year))
  fromPathPiece = yearFromText

yearFromText :: Text -> Maybe Year
yearFromText text =
  case splitString (unpack text) of
    Just strings -> yearFromStrings strings
    Nothing -> Nothing

validateYearRange :: Integer -> Integer -> Maybe Year
validateYearRange start end = if start + 1 == end then Just $ Year start end else Nothing

yearFromStrings :: (String, String) -> Maybe Year
yearFromStrings value =
  case (textToInteger *** textToInteger) value of
    (Just start, Just end) -> validateYearRange start end
    _ -> Nothing

splitString :: String -> Maybe (String, String)
splitString [a, b, c, d, e, f, g, h] = Just ([a, b, c, d], [e, f, g, h])
splitString _ = Nothing

textToInteger :: String -> Maybe Integer
textToInteger value = readMaybe value :: Maybe Integer
