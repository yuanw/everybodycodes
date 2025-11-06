module Y2023.Day6 where

import Data.List (foldl')

type Time = Int
type Dist = Int
data Race = Race Time Dist

possibleWay :: Race -> Int
possibleWay (Race t d) = length [i | i <- [1 .. t - 1], raceF i > d]
  where
    raceF :: Time -> Dist
    raceF n = (t - n) * n

testRaces :: [Race]
testRaces = [Race 7 9, Race 15 40, Race 30 200]

-- Time:        44     80     65     72
-- Distance:   208   1581   1050   1102

races :: [Race]
races = [Race 44 208, Race 80 1581, Race 65 1050, Race 72 1102]

-- --34278221
-- race :: Race
-- race = Race 44806572 208158110501102
-- --71503
race :: Race
race = Race 71530 940200

getWays :: Int -> Int -> Int
getWays tMax dRecord = isqrt (tMax * tMax - 4 * dRecord)

-- | integer square root
isqrt :: Int -> Int
isqrt n
    | n < 2 = n
    | otherwise = go n
  where
    go !x
        | x == y = x
        | otherwise = go y
      where
        y = (x + n `div` x) `div` 2

partI :: IO ()
partI = do
    print (foldl' (\accum r -> accum * (possibleWay r)) 1 races)

partII :: IO ()
partII = do
    print (getWays 71530 940200)
