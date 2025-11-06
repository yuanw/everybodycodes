module Y2022.Day8 where

import Data.Char (digitToInt)

type Grid = [[Int]]
type Point = (Int, Int)

get :: Point -> Grid -> Int
get (x, y) = (!! y) . (!! x)

getWidth :: Grid -> Int
getWidth = length . (!! 0)

getLength :: Grid -> Int
getLength = length

onEdge :: Point -> Grid -> Bool
onEdge (x, y) grid = x == 0 || y == 0 || getWidth grid - 1 == y || getLength grid - 1 == x

goLeft :: Point -> Point
goLeft (x, y) = (x, y - 1)

goRight :: Point -> Point
goRight (x, y) = (x, y + 1)

goTop :: Point -> Point
goTop (x, y) = (x - 1, y)

goBottom :: Point -> Point
goBottom (x, y) = (x + 1, y)

isVisibleFromLeft :: Point -> Grid -> Bool
isVisibleFromLeft (_, 0) _ = True
isVisibleFromLeft (x, y) grid = let val = get (x, y) grid in all (val >) [get (x, i) grid | i <- [0 .. y - 1]]

viewDistanceFromLeft :: Point -> Grid -> Int
viewDistanceFromLeft (_, 0) _ = 0
viewDistanceFromLeft point grid = helper 0 (get point grid) (goLeft point) grid
  where
    helper :: Int -> Int -> Point -> Grid -> Int
    helper acc val p g = let val' = get p g in if val' >= val || onEdge p grid then acc + 1 else helper (acc + 1) val (goLeft p) grid

isVisibleFromRight :: Point -> Grid -> Bool
isVisibleFromRight (x, y) grid = y == getWidth grid - 1 || let val = get (x, y) grid in all (val >) [get (x, i) grid | i <- [y + 1 .. getWidth grid - 1]]

viewDistanceFromRight :: Point -> Grid -> Int
viewDistanceFromRight point grid = if onEdge point grid then 0 else helper 0 (get point grid) (goRight point) grid
  where
    helper :: Int -> Int -> Point -> Grid -> Int
    helper acc val p g = let val' = get p g in if val' >= val || onEdge p grid then acc + 1 else helper (acc + 1) val (goRight p) grid

isVisibleFromTop :: Point -> Grid -> Bool
isVisibleFromTop (0, _) _ = True
isVisibleFromTop (x, y) grid = let val = get (x, y) grid in all (val >) [get (i, y) grid | i <- [0 .. x - 1]]

viewDistanceFromTop :: Point -> Grid -> Int
viewDistanceFromTop point grid = if onEdge point grid then 0 else helper 0 (get point grid) (goTop point) grid
  where
    helper :: Int -> Int -> Point -> Grid -> Int
    helper acc val p g = let val' = get p g in if val' >= val || onEdge p grid then acc + 1 else helper (acc + 1) val (goTop p) grid

isVisibleFromBottom :: Point -> Grid -> Bool
isVisibleFromBottom (x, y) grid = x == getLength grid - 1 || let val = get (x, y) grid in all (val >) [get (i, y) grid | i <- [x + 1 .. getLength grid - 1]]

viewDistanceFromBottom :: Point -> Grid -> Int
viewDistanceFromBottom point grid = if onEdge point grid then 0 else helper 0 (get point grid) (goBottom point) grid
  where
    helper :: Int -> Int -> Point -> Grid -> Int
    helper acc val p g = let val' = get p g in if val' >= val || onEdge p grid then acc + 1 else helper (acc + 1) val (goBottom p) grid

isVisible :: Point -> Grid -> Bool
isVisible point grid = isVisibleFromLeft point grid || isVisibleFromRight point grid || isVisibleFromTop point grid || isVisibleFromBottom point grid

-- we should be able skip edge
countVisible :: Grid -> Int
countVisible grid =
    length
        [ (x, y) | x <- [0 .. getLength grid - 1], y <- [0 .. getWidth grid - 1], isVisible (x, y) grid
        ]

maxScore :: Grid -> Int
maxScore grid =
    maximum
        [ scenicScore (x, y) grid | x <- [0 .. getLength grid - 1], y <- [0 .. getWidth grid - 1]
        ]

scenicScore :: Point -> Grid -> Int
scenicScore point grid = if onEdge point grid then 0 else viewDistanceFromLeft point grid * viewDistanceFromRight point grid * viewDistanceFromTop point grid * viewDistanceFromBottom point grid

readInput :: FilePath -> IO Grid
readInput file = map (map digitToInt) . lines <$> readFile file

testGrid :: IO Grid
testGrid = readInput "data/2022/day8.txt"

partII :: IO ()
partII = do
    grid <- testGrid
    print $ maxScore grid

partI :: IO Int
partI = do
    grid <- testGrid
    return $ countVisible grid
