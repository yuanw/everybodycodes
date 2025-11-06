module Y2022.Day4 (parse, partI, test, partII) where

import Data.List.Split (splitOn)
import Data.Set qualified as Set

type Pair = (Int, Int)
testInput :: String
testInput =
    "\
    \2-4,6-8\n\
    \2-3,4-5\n\
    \5-7,7-9\n\
    \2-8,3-7\n\
    \6-6,4-6\n\
    \2-6,4-8\n"
isContain :: Pair -> Pair -> Bool
isContain (x, y) (x', y') = (x <= x' && y >= y') || (x' <= x && y' >= y)

overlap :: Pair -> Pair -> Bool
overlap (x, y) (x', y') = Set.size (Set.fromList [x .. y] `Set.intersection` Set.fromList [x' .. y']) > 0

parse :: String -> (Pair, Pair)
parse input = (parsePair one, parsePair two)
  where
    sByComm = splitOn ","
    sByI = splitOn "-"
    sec v = v !! 1
    one = head . sByComm $ input
    two = sec . sByComm $ input
    parsePair :: String -> Pair
    parsePair s = (read . head . sByI $ s, read . sec . sByI $ s)

solution1 :: String -> Int
solution1 = length . filter (uncurry isContain) . map parse . lines

solution2 :: String -> Int
solution2 = length . filter (uncurry overlap) . map parse . lines

partI :: IO Int
partI = solution1 <$> readFile "data/2022/day4.txt"

partII :: IO Int
partII = solution2 <$> readFile "data/2022/day4.txt"

-- test :: Int
test = solution1 testInput
