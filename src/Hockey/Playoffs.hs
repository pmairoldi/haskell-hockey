module Hockey.Playoffs (
    Seed(..),
    seeds
)

where

import Data.List as List
import Hockey.Types (Year)

data Seed = Seed {
    year :: Int,
    conference :: String,
    round :: Int,
    seed :: Int,
    homeId :: String,
    awayId :: String
} deriving (Show, Read)

generateSeed :: Int -> Int -> [Int] -> [Seed]
generateSeed year round xs = List.map (\x -> (Seed year "e" round x "" "")) xs ++ List.map (\x -> (Seed year "w" round x "" "")) xs

seeds' :: Int -> [Int] -> [Seed]
seeds' year (x:xs)
    | x == 1 = (generateSeed year x [1..4]) ++ (seeds' year xs)
    | x == 2 = (generateSeed year x [1..2]) ++ (seeds' year xs)
    | x == 3 = (generateSeed year x [1]) ++ (seeds' year xs)
    | x == 4 = [Seed year "f" 4 1 "" ""]

seeds :: Year -> [Seed]
seeds year = seeds' (read (show (fst year)) :: Int) [1..4]
