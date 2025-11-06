{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day9 where

import Control.Applicative (many)
import Data.Attoparsec.Text
import Data.Foldable (all)
import Data.Text.IO qualified as TIO

type Input = [[Int]]

rowParser :: Parser [Int]
rowParser = do
    nums <- (signed decimal) `sepBy` (char ' ')
    return nums

inputParser :: Parser [[Int]]
inputParser = many $ rowParser <* endOfLine

findSeq :: [Int] -> [[Int]]
findSeq a = f a []
  where
    h :: [Int] -> [Int]
    h n = [n !! i - n !! (i - 1) | i <- [1 .. length n - 1]]

    reachEnd :: [Int] -> Bool
    reachEnd = all (== 0)

    f :: [Int] -> [[Int]] -> [[Int]]
    f s accum = if reachEnd s then accum else f (h s) (accum ++ [s])

-- binomial coefficient, eg choose k from n
choose :: Int -> Int -> Int
choose n 0 = 1
choose n 1 = n
choose 0 _ = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

-- https://www.geeksforgeeks.org/newton-forward-backward-interpolation/
-- https://www.youtube.com/watch?app=desktop&v=4AuV93LOPcE&t=1679s
forward :: Int -> [Int] -> Int
forward n row = sum $ zipWith (\x y -> x * choose n y) row [0 ..]

-- https://atozmath.com/example/CONM/NumeInterPola.aspx?q=B&q1=E1
findPrev :: [Int] -> Int
findPrev = findNext . reverse

findNext :: [Int] -> Int
findNext numbers = forward (length numbers) factors
  where
    seqs = findSeq numbers
    factors = map head seqs

-- 1743490457
partI :: IO ()
partI = do
    rawInput <- TIO.readFile "data/2023/day9.txt"
    let e = parseOnly inputParser rawInput
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right input -> print (sum $ map findNext input)

-- 1053
partII :: IO ()
partII = do
    rawInput <- TIO.readFile "data/2023/day9.txt"
    let e = parseOnly inputParser rawInput
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right input -> print (sum $ map findPrev input)
