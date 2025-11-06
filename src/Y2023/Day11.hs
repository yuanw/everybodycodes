module Y2023.Day11 where

import Data.Foldable (forM_)
import Data.IntSet qualified as IS
import Data.List (subsequences, tails)
import Data.Set.NonEmpty (NESet)

type Grid = [String]
type Point = (Int, Int)

rowsNoGalaxies :: Grid -> [Int]
rowsNoGalaxies grid = [i | i <- [0 .. (rows - 1)], '#' `notElem` (grid !! i)]
  where
    rows = length grid

colsNoGalaxies :: Grid -> [Int]
colsNoGalaxies grid = [i | i <- [0 .. (cols - 1)], '#' `notElem` [(grid !! j) !! i | j <- [0 .. (rows - 1)]]]
  where
    rows = length grid
    cols = length (head grid)

expand :: Int -> Grid -> Grid
expand size grid = expandCol (expandRow grid)
  where
    expandCol :: Grid -> Grid
    expandCol = map expandRowByRow
    cols = length (head grid)
    expandRowByRow row = foldl (\r i -> take i r ++ replicate size '.' ++ drop i r) row colNeedExpand
    colNeedExpand = zipWith (\a b -> a + (b * size)) (colsNoGalaxies grid) [0 ..]

    expandRow :: Grid -> Grid
    expandRow g = foldl (\g' i -> take i g' ++ replicate size (replicate cols '.') ++ drop i g') g rowsNeedExpand
    rowsNeedExpand = zipWith (\a b -> a + (b * size)) (rowsNoGalaxies grid) [0 ..]

drawGrid :: (Show a) => [[a]] -> IO ()
drawGrid grid = forM_ grid (\row -> putStr (filter (\c -> c /= '\'' && c /= '"') $ concatMap show row) >> putStr "\n") >> putStr "\n"

getCharFromGrid :: Grid -> Point -> Char
getCharFromGrid grid (x, y) = (grid !! x) !! y

findGalaxies :: Grid -> [Point]
findGalaxies grid = [(i, j) | i <- [0 .. (rows - 1)], j <- [0 .. (cols - 1)], getCharFromGrid grid (i, j) == '#']
  where
    rows = length grid
    cols = length (head grid)

allCombinations :: [Point] -> [(Point, Point)]
-- allCombinations = map (\ps -> (head ps, ps !! 1))  . filter ( (== 2) . length)  . subsequences
allCombinations = map (\ps -> (head ps, ps !! 1)) . subsequencesOfSize 2
manhattanDis :: Point -> Point -> Int
manhattanDis (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
    let l = length xs
     in if (n > l)
            then []
            else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs') =
        let next = subsequencesBySize xs'
         in zipWith
                (++)
                ([] : next)
                (map (map (x :)) next ++ [[]])

partI :: IO ()
partI = do
    grid <- lines <$> readFile "data/2023/day11-test.txt"
    -- let grid' = expand (1000000 - 1) grid
    -- drawGrid grid
    -- drawGrid (expand 1 grid)
    -- drawGrid grid'
    print (findGalaxies grid)
    print (findGalaxies $ expand 1 grid)
    print (findGalaxies $ expand 9 grid)
    print (findGalaxies $ expand 99 grid)

-- print (manhattanDis (6, 1) (11, 5))
-- let ds = map (uncurry manhattanDis) . allCombinations $ findGalaxies grid'
-- print (sum ds)
