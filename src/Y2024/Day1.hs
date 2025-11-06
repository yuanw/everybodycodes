module Y2024.Day1 (partI, partII) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

readTuple :: String -> (Int, Int)
readTuple txt = (read $ head parts, read $ last parts)
  where
    parts = words txt

start :: ([Int], [Int])
start = ([], [])

readInput :: String -> [(Int, Int)]
readInput = map readTuple . init . splitOn "\n"

foldStep :: [(Int, Int)] -> ([Int], [Int])
foldStep = foldr (\(a, b) (c, d) -> (a : c, b : d)) start

foldStep' :: [(Int, Int)] -> (M.Map Int Int, M.Map Int Int)
foldStep' = foldr (\(a, b) (c, d) -> (M.insertWith (+) a 1 c, M.insertWith (+) b 1 d)) (M.empty, M.empty)

sortStep :: ([Int], [Int]) -> ([Int], [Int])
sortStep (a, b) = (sort a, sort b)

zipStep :: ([Int], [Int]) -> [(Int, Int)]
zipStep (a, b) = zip a b

accumMap :: M.Map Int Int -> M.Map Int Int -> Int
accumMap m1 m2 = sum . map (\(k, v) -> if M.member k m2 then k * v * (fromMaybe 0 $ M.lookup k m2) else 0) $ M.assocs m1

partI :: IO ()
partI = do
    a <- sum . map (\(a, b) -> abs (a - b)) . zipStep . sortStep . foldStep . readInput <$> readFile "data/2024/day1.txt"
    print a

partII :: IO ()
partII = do
    a <- uncurry accumMap . foldStep' . readInput <$> readFile "data/2024/day1.txt"
    print a
