module Y2022.Day6 where

import Data.Set qualified as Set

type Point = (Int, Int)
data TwoPointer = TwoPointer {_start :: Int, _end :: Int, _val :: String} deriving (Show)

limit :: Int
limit = 4

msgLimit :: Int
msgLimit = 14

-- findMarker :: TwoPointer -> Bool
-- findMarker = (== limit) . Set.size . Set.fromList . _val

-- findMsgMarker :: TwoPointer -> Bool
-- findMsgMarker = (== msgLimit) . Set.size . Set.fromList . _val

empty :: TwoPointer
empty = TwoPointer 0 0 ""

find :: Int -> TwoPointer -> Bool
find l = (== l) . Set.size . Set.fromList . _val

nextPoint :: Int -> TwoPointer -> Char -> TwoPointer
nextPoint l (TwoPointer s e v) c = let reach = length v == l in TwoPointer (if reach then s + 1 else s) (e + 1) ((if reach then tail v else v) ++ [c])

firstMarker :: Int -> String -> TwoPointer
firstMarker l = foldl (\t c -> if find l t then t else nextPoint l t c) empty

test :: [TwoPointer]
test =
    map
        (firstMarker limit)
        [ "bvwbjplbgvbhsrlpgdmjqwftvncz"
        , "nppdvjthqldpwncqszvftbrmjlhg"
        , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        ]
test2 :: [TwoPointer]
test2 =
    map
        (firstMarker msgLimit)
        [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        , "bvwbjplbgvbhsrlpgdmjqwftvncz"
        , "nppdvjthqldpwncqszvftbrmjlhg"
        , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        ]

partI :: IO TwoPointer
partI = firstMarker limit <$> readFile "data/2022/day6.txt"

partII :: IO TwoPointer
partII = firstMarker msgLimit <$> readFile "data/2022/day6.txt"
