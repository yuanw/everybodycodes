module Y2023.Day3 (partI, partII) where

import Control.Monad (join)
import Data.Char (isDigit)

type Grid = [[Char]]
type Point = (Int, Int)

data State = State {currentNum :: [Point], allNum :: [[Point]]} deriving (Show)
zero :: State
zero = State [] []

lookUp :: Point -> Grid -> Char
lookUp (x, y) g = (g !! y) !! x

appendChar :: Point -> Grid -> State -> State
appendChar p g (State cN aL) = if isDigit c then State (p : cN) aL else State [] (if null cN then aL else aL ++ [cN])
  where
    c = lookUp p g

lvlFold :: State -> Grid -> Int -> Int -> State
lvlFold s grid y width = cleanUp $ foldl (\s' p -> appendChar p grid s') s [(i, y) | i <- [0 .. width - 1]]
  where
    cleanUp :: State -> State
    cleanUp (State n allN) = State [] (if null n then allN else allN ++ [n])

gridFold :: State -> Grid -> Int -> Int -> State
gridFold s grid heigh width = foldl (\s' y -> lvlFold s' grid y width) s [0 .. heigh - 1]

getNumbers :: [[Point]] -> Grid -> [Int]
getNumbers ps grid = map helper ps
  where
    helper :: [Point] -> Int
    helper points = read $ foldl (\s point -> lookUp point grid : s) "" points

getNeighour :: Int -> Int -> Point -> [Point]
getNeighour heigh width (x, y) =
    filter (\(a, b) -> a >= 0 && a < width && b >= 0 && b < heigh) $
        map
            (\(a, b) -> (a + x, b + y))
            [ (-1, -1)
            , (0, -1)
            , (1, -1)
            , (-1, 0)
            , (1, 0)
            , (-1, 1)
            , (0, 1)
            , (1, 1)
            ]

adjacentToSymbol :: Int -> Int -> Grid -> [Point] -> Bool
adjacentToSymbol heigh width grid points = any ((\c -> not (isDigit c || c == '.')) . flip lookUp grid) $ join (map (getNeighour heigh width) points)

adjacentToNumbers :: Point -> Int -> Int -> [[Point]] -> [[Point]]
adjacentToNumbers p h w = filter (any (`elem` ns))
  where
    ns = getNeighour h w p

findStarPoint :: Grid -> Int -> Int -> [Point]
findStarPoint grid height width = [(i, j) | i <- [0 .. width - 1], j <- [0 .. height - 1], lookUp (i, j) grid == '*']

partI :: IO ()
partI = do
    grid <- lines <$> readFile "data/2023/day3.txt"
    let y = length grid
        x = length (head grid)

        state = gridFold zero grid y x
        pointOfNum = allNum state
        wantedPointOfNum = filter (adjacentToSymbol y x grid) pointOfNum
    -- print pointOfNum
    -- print (test y x grid [(2,0),(1,0),(0,0)])
    -- print (adjacentToSymbol   y x grid [(2,0),(1,0),(0,0)])
    -- print (findStarPoint grid y x)
    print (sum $ getNumbers wantedPointOfNum grid)

partII :: IO ()
partII = do
    grid <- lines <$> readFile "data/2023/day3.txt"
    let y = length grid
        x = length (head grid)

        state = gridFold zero grid y x
        pointOfNum = allNum state
        stars = findStarPoint grid y x
        -- gears =  map (\p -> adjacentToNumbers p y x grid pointOfNum ) stars
        gears = filter (\l -> length l == 2) $ map (\p -> adjacentToNumbers p y x pointOfNum) stars
        partNumbers = sum $ map (product . (`getNumbers` grid)) gears

    print partNumbers
