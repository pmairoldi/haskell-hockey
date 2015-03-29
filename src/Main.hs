{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- My Modules
import Hockey.Requests
import Hockey.Types
import Hockey.Formatting
import Hockey.Parsing

-- Load from environment variables
currentYear :: Integer
currentYear = 2014

currentSeason :: Season
currentSeason = Playoffs

main :: IO()
main = do
    results <- getResults $ dateFromComponents 2015 3 29
    print results
