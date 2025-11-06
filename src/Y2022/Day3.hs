module Y2022.Day3 (testInput, solution1, solution2, partI, partII) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

priorityMap :: Map.Map Char Int
priorityMap = Map.fromList (zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52])

testInput :: String
testInput =
    "\
    \vJrwpWtwJgWrhcsFMMfFFhFp\n\
    \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
    \PmmdzqPrVvPwwTWBwg\n\
    \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
    \ttgJtRGJQctTZtZT\n\
    \CrZsJsPPZsGzwwsLwLmpwMDw\n"

type Rucksack = String

split :: String -> (Rucksack, Rucksack)
split input = splitAt a input
  where
    a = length input `div` 2

shareItems :: (Rucksack, Rucksack) -> Rucksack
-- shareItems (first, sec) = [ i | i <- first,  i `elem` sec ]
shareItems (f, s) = foldr (\x y -> if x `elem` s && notElem x y then x : y else y) "" f
solution1 :: String -> Int
solution1 = foldr (\x y -> fromMaybe 0 (Map.lookup x priorityMap) + y) 0 . concatMap (shareItems . split) . lines

splitPart2 :: [String] -> [(Rucksack, Rucksack, Rucksack)]
splitPart2 input = case splitAt 3 input of
    (x, []) -> [f x]
    (x, y) -> f x : splitPart2 y
  where
    f :: [String] -> (Rucksack, Rucksack, Rucksack)
    f i = (head i, i !! 1, i !! 2)

shareItems' :: (Rucksack, Rucksack, Rucksack) -> Rucksack
shareItems' (f, s, t) = foldr (\x y -> if x `elem` s && x `elem` t && notElem x y then x : y else y) "" f

solution2 :: String -> Int
solution2 = sum . map (foldr (\x y -> fromMaybe 0 (Map.lookup x priorityMap) + y) 0 . shareItems') . splitPart2 . lines

partI :: IO Int
partI = solution1 <$> readFile "data/2022/day3.txt"

partII :: IO Int
partII = solution2 <$> readFile "data/2022/day3.txt"
