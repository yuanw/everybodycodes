{-# LANGUAGE DerivingStrategies #-}

module Y2022.Day2 (score, Move (..), Round (..), partI, predicate, partII) where

import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe)

data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord, Enum, Bounded)

{- | @outcome our_move their_move@
 outcome :: Move -> Move -> Outcome
 outcome Rock Scissors = Win
 outcome Paper Rock = Win
 outcome Scissors Paper = Win
 outcome us them
     | us == them = Tie
     | otherwise = Lose
-}
outcome' :: Move -> Move -> Outcome
outcome' mine their = toEnum ((fromEnum mine - fromEnum their + 1) `mod` 3)

data Round = Round {getElf :: Move, getMine :: Move}

score :: Round -> Int
score run = o + shape
  where
    o = case outcome' (getMine run) (getElf run) of
        Win -> 6
        Lose -> 0
        Tie -> 3

    shape = case getMine run of
        Rock -> 1
        Paper -> 2
        Scissors -> 3

-- A for Rock, B for Paper, and C for Scissors.
-- X for Rock, Y for Paper, and Z for Scissors.
readMove :: String -> Maybe Move
readMove "A" = Just Rock
readMove "B" = Just Paper
readMove "C" = Just Scissors
readMove "X" = Just Rock
readMove "Y" = Just Paper
readMove "Z" = Just Scissors
readMove _ = Nothing

readRound :: String -> Maybe Round
readRound input = liftA2 Round (readMove e) (readMove m)
  where
    a = words input
    e = head a
    m = a !! 1

readRound' :: String -> Maybe Round'
readRound' input = liftA2 Round' (readMove e) (readOutcome m)
  where
    a = words input
    e = head a
    m = a !! 1

-- X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
readInput :: FilePath -> IO [Round]
readInput path = mapMaybe readRound . lines <$> readFile path

readOutcome :: String -> Maybe Outcome
readOutcome "X" = Just Lose
readOutcome "Y" = Just Tie
readOutcome "Z" = Just Win
readOutcome _ = Nothing

data Round' = Round' {getTheir :: Move, getOutcome :: Outcome}

predicate :: Move -> Outcome -> Move
predicate their o = toEnum ((fromEnum o + fromEnum their - 1) `mod` 3)

score' :: Round' -> Int
score' r = (fromEnum m + 1) + (fromEnum o * 3)
  where
    o = getOutcome r
    m = predicate (getTheir r) (getOutcome r)

partI :: IO Int
partI = sum . map score <$> readInput "data/2022/day2.txt"

partII :: IO Int
partII = sum . map score' . mapMaybe readRound' . lines <$> readFile "data/2022/day2.txt"
