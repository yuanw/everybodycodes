{-# LANGUAGE ViewPatterns #-}

module Y2023.Day1 where

import Control.Arrow ((&&&))
import Data.Char (digitToInt, isDigit)
import Data.List (isSuffixOf, stripPrefix)
import Data.List.Split (splitOn)

readFromLeft :: String -> Int
readFromLeft (c : cs) = if isDigit c then digitToInt c else readFromLeft cs
readFromLeft [] = error "unexpected empty string"

readFromRight :: String -> Int
readFromRight [] = error "unexpected empty string"
readFromRight s =
    let c = last s
        cs = init s
     in if isDigit c then digitToInt c else readFromRight cs

readInt :: String -> Int
readInt = (\(x, y) -> x * 10 + y) . (readFromLeft &&& readFromRight)

readFromLeft' :: String -> Int
readFromLeft' (stripPrefix "one" -> Just _) = 1
readFromLeft' (stripPrefix "two" -> Just _) = 2
readFromLeft' (stripPrefix "three" -> Just _) = 3
readFromLeft' (stripPrefix "four" -> Just _) = 4
readFromLeft' (stripPrefix "five" -> Just _) = 5
readFromLeft' (stripPrefix "six" -> Just _) = 6
readFromLeft' (stripPrefix "seven" -> Just _) = 7
readFromLeft' (stripPrefix "eight" -> Just _) = 8
readFromLeft' (stripPrefix "nine" -> Just _) = 9
readFromLeft' (c : cs) = if isDigit c then digitToInt c else readFromLeft' cs
readFromLeft' [] = error "unexpected empty string"

readFromRight' :: String -> Int
readFromRight' (isSuffixOf "one" -> True) = 1
readFromRight' (isSuffixOf "two" -> True) = 2
readFromRight' (isSuffixOf "three" -> True) = 3
readFromRight' (isSuffixOf "four" -> True) = 4
readFromRight' (isSuffixOf "five" -> True) = 5
readFromRight' (isSuffixOf "six" -> True) = 6
readFromRight' (isSuffixOf "seven" -> True) = 7
readFromRight' (isSuffixOf "eight" -> True) = 8
readFromRight' (isSuffixOf "nine" -> True) = 9
readFromRight' [] = error "unexpected empty string"
readFromRight' s =
    let c = last s
        cs = init s
     in if isDigit c then digitToInt c else readFromRight' cs

readInt' :: String -> Int
readInt' = (\(x, y) -> x * 10 + y) . (readFromLeft' &&& readFromRight')

partII :: IO ()
partII = do
    a <- sum . map readInt' . filter (not . null) . splitOn "\n" <$> readFile "data/2023/day1.txt"
    print a

partI :: IO ()
partI = do
    a <- sum . map readInt . filter (not . null) . splitOn "\n" <$> readFile "data/2023/day1.txt"
    print a
