module Hockey.Playoffs (
    Seed(..),
    seeds,
    updateSeeds,

    updateSeedsWithWildcards,
    groupWinningSeeds,
    winningTeamsToPlayoffSeed,
    groupWinningTeams,
    exractWinningTeam,
    filterWinningSeed
)

where

import Data.List as List
import Hockey.Types (Year)
import Hockey.Database.Types
import Hockey.Formatting (integerToInt)

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

updateSeeds :: Year -> [(String, PlayoffSeed)] -> [PlayoffSeed]
updateSeeds year winningSeeds
    | year >= (2013, 2014) = updateSeedsWithWildcards (integerToInt (fst year)) winningSeeds
    | otherwise = [] -- todo add back other years

-- 2013-2014 - Present format
filterWinningSeed :: (String, PlayoffSeed) -> String -> Int -> [Int] -> Bool
filterWinningSeed winningSeed conference round series
    | (playoffSeedRound seed) == 3 && (playoffSeedRound seed) == round && (playoffSeedSeries seed) `elem` series = True
    | (playoffSeedConference seed) == conference && (playoffSeedRound seed) == round && (playoffSeedSeries seed) `elem` series = True
    | otherwise = False
    where seed = (snd winningSeed)

exractWinningTeam :: (String, PlayoffSeed) -> (String, Int)
exractWinningTeam winningSeed
    | team == (playoffSeedAwayId seed) = (team, (playoffSeedAwaySeed seed))
    | team == (playoffSeedHomeId seed) = (team, (playoffSeedHomeSeed seed))
    where
        team = (fst winningSeed)
        seed = (snd winningSeed)

groupWinningTeams :: Int -> [(String, PlayoffSeed)] -> (Maybe (String, Int), Maybe (String, Int))
groupWinningTeams round [] = (Nothing, Nothing)
groupWinningTeams 3 [x]
    | conference == "w" = (Just team, Nothing)
    | otherwise = (Nothing, Just team)
    where
        team = exractWinningTeam x
        conference = playoffSeedConference (snd x)
groupWinningTeams 3 teams
    | teamOneConference == "w" = (Just teamOne, Just teamTwo)
    | otherwise = (Just teamTwo, Just teamOne)
    where
        teamOne = exractWinningTeam (teams !! 0)
        teamTwo = exractWinningTeam (teams !! 1)
        teamOneConference = playoffSeedConference (snd (teams !! 0))
groupWinningTeams round [x] = (Just (exractWinningTeam x), Nothing)
groupWinningTeams round teams
    | (snd teamOne) <= (snd teamTwo) = (Just teamOne, Just teamTwo)
    | (snd teamOne) > (snd teamTwo) = (Just teamTwo, Just teamOne)
    where
        teamOne = exractWinningTeam (teams !! 0)
        teamTwo = exractWinningTeam (teams !! 1)

winningTeamsToPlayoffSeed :: (Maybe (String, Int), Maybe (String, Int)) -> Int -> String -> Int -> Int -> [PlayoffSeed]
winningTeamsToPlayoffSeed teams year conference round series =
    case teams of
    (Just (x), Just (y)) -> [PlayoffSeed year conference round series (fst x) (fst y) (snd x) (snd y)]
    (Just (x), Nothing) -> [PlayoffSeed year conference round series (fst x) [] (snd x) 0]
    (Nothing, Just (y)) -> [PlayoffSeed year conference round series [] (fst y) 0 (snd y)]
    otherwise -> []

groupWinningSeeds :: [(String, PlayoffSeed)] -> Int -> String -> Int -> [Int] ->  [PlayoffSeed]
groupWinningSeeds winningSeeds year conference round series
    | round == 1 && series == [1,2] = winningTeamsToPlayoffSeed teams year conference 2 1
    | round == 1 && series == [3,4] = winningTeamsToPlayoffSeed teams year conference 2 2
    | round == 2 = winningTeamsToPlayoffSeed teams year conference 3 1
    | round == 3 = winningTeamsToPlayoffSeed teams year "f" 4 1
    | otherwise = []
    where teams = groupWinningTeams round $ (List.filter (\x -> filterWinningSeed x conference round series) winningSeeds)

updateSeedsWithWildcards :: Int -> [(String, PlayoffSeed)] ->  [PlayoffSeed]
updateSeedsWithWildcards year winningSeeds =
    (groupWinningSeeds winningSeeds year "e" 1 [1,2]) ++
    (groupWinningSeeds winningSeeds year "e" 1 [3,4]) ++
    (groupWinningSeeds winningSeeds year "w" 1 [1,2]) ++
    (groupWinningSeeds winningSeeds year "w" 1 [3,4]) ++
    (groupWinningSeeds winningSeeds year "e" 2 [1,2]) ++
    (groupWinningSeeds winningSeeds year "w" 2 [1,2]) ++
    (groupWinningSeeds winningSeeds year [] 3 [1])
