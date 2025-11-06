module Y2023.Day10 where

import Control.Monad

-- import Control.Monad.IO.Class

-- import Control.Monad.State
import Control.Monad.Trans.State
import Data.List (groupBy, sortOn, unfoldr)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as S

type Point = (Int, Int)
type Grid = [String]

type Graph = Map Point [Point]

data Dir = N | S | W | E deriving (Show)

data Path = Path {getCurrent :: Point, getPrev :: Maybe Point} deriving (Show)

{- | is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.
S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
-}
readGraph :: Grid -> Graph
readGraph grid = M.fromList . filter (\(_, ps) -> not (null ps)) . map (\(p, ps) -> (p, filter (\(x, y) -> 0 <= x && x < row && 0 <= y && y < col) ps)) $ [adjList ((grid !! i) !! j) (i, j) | i <- [0 .. row - 1], j <- [0 .. col - 1]]
  where
    col = length $ head grid
    row = length grid

    adjList :: Char -> Point -> (Point, [Point])
    adjList 'S' (x, y) = ((x, y), [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)])
    adjList '|' (x, y) = ((x, y), [(x - 1, y), (x + 1, y)])
    adjList '-' (x, y) = ((x, y), [(x, y - 1), (x, y + 1)])
    adjList 'L' (x, y) = ((x, y), [(x - 1, y), (x, y + 1)])
    adjList 'J' (x, y) = ((x, y), [(x - 1, y), (x, y - 1)])
    adjList '7' (x, y) = ((x, y), [(x, y - 1), (x + 1, y)])
    adjList 'F' (x, y) = ((x, y), [(x, y + 1), (x + 1, y)])
    adjList _ p = (p, [])

-- level = {s: 0}
-- parent = {s: None}
-- frontiers = [s]
-- i = i
-- while frontiers:
--     next_nodes = []
--     for u in froniters:
--         for v in Adj[u]:
--             if v not in level:
--                 level[v] = i
--                 parent[v] = u
--                 next_nodes.append(v)
--     froniters = next_nodes
--     i += 1
data MazeState = MazeState Grid (Map Point Int) Graph Int Point
startMazeState :: Grid -> Point -> Point -> Point -> MazeState
startMazeState grid s x y = MazeState grid (M.fromList [(s, 0), (x, 1)]) (readGraph grid) 2 y

bfs :: [Point] -> StateT MazeState IO (Map Point Int, Int)
bfs [] = do
    MazeState g m _ _ d <- get
    return (m, fromMaybe 0 (M.lookup d m))
bfs frontiers = do
    MazeState grid lvlMap g rd dest <- get
    -- _ <- liftIO $ print ("lvl " <> (show rd))
    let nextNodes = filter (`M.member` g) $ frontiers >>= (\p -> fromMaybe [] $ M.lookup p g)
        (newLvlMap, newFrontiers) = foldl (\(m, nf) p -> if M.member p m then (m, nf) else (M.insert p rd m, p : nf)) (lvlMap, []) nextNodes :: (Map Point Int, [Point])

    -- _ <- liftIO $ print ("frontiers  " <> show frontiers)
    -- _ <- liftIO $ print ("nextNodes " <> show nextNodes)
    -- _ <- liftIO $ print ("newFrontiers " <> show newFrontiers)
    -- _ <- liftIO $ drawGrid (fillGrid grid newLvlMap  )
    put (MazeState grid newLvlMap g (rd + 1) dest)
    bfs newFrontiers

fillGrid :: Grid -> Map Point Int -> [[String]]
fillGrid grid lvlMap = map (map snd) sorted
  where
    sorted = map (sortOn (snd . fst)) sortedByRow :: [[(Point, String)]]
    sortedByRow = groupBy (\a b -> (fst (fst a)) == (fst (fst b))) tupleList :: [[(Point, String)]]
    tupleList = [((i, j), maybe "." (show) (M.lookup (i, j) lvlMap)) | i <- [0 .. row - 1], j <- [0 .. col - 1]] :: [(Point, String)]
    col = length $ head grid
    row = length grid

getCharFromGrid :: Grid -> Point -> Char
getCharFromGrid grid (x, y) = (grid !! x) !! y

startPoint :: Grid -> Point
startPoint grid = head [(i, j) | i <- [0 .. row - 1], j <- [0 .. col - 1], getCharFromGrid grid (i, j) == 'S']
  where
    col = length $ head grid
    row = length grid

move :: Point -> Dir -> Point
move (x, y) N = (x - 1, y)
move (x, y) S = (x + 1, y)
move (x, y) W = (x, y - 1)
move (x, y) E = (x, y + 1)

isBound :: Point -> Grid -> Bool
isBound (x, y) grid = (0 <= x) && (x < row) && (0 <= y) && (y < col)
  where
    col = length $ head grid
    row = length grid

{- | is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.
S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
-}
possiblePath :: (Point, Dir, Char) -> Bool
possiblePath (_, S, '|') = True
possiblePath (_, N, '|') = True
possiblePath (_, E, '-') = True
possiblePath (_, W, '-') = True
possiblePath (_, S, 'L') = True
possiblePath (_, W, 'L') = True
possiblePath (_, S, 'J') = True
possiblePath (_, E, 'J') = True
possiblePath (_, N, '7') = True
possiblePath (_, E, '7') = True
possiblePath (_, N, 'F') = True
possiblePath (_, W, 'F') = True
-- possiblePath (_, _ , 'S') = True
possiblePath _ = False

neibours :: Point -> Grid -> [(Point, Dir, Char)]
neibours p grid = filter possiblePath . map (\(p, d) -> (p, d, getCharFromGrid grid p)) . filter ((`isBound` grid) . fst) $ map (\d -> (move p d, d)) dirs
  where
    dirs = [N, S, W, E]

drawGrid :: (Show a) => [[a]] -> IO ()
drawGrid grid = forM_ grid (\row -> putStr (filter (\c -> c /= '\'' && c /= '"') $ concatMap show row) >> putStr "\n") >> putStr "\n"
testInput1 :: String
testInput1 =
    "\
    \.....\n\
    \.S-7.\n\
    \.|.|.\n\
    \.L-J.\n\
    \.....\n"

testInput2 :: String
testInput2 =
    "\
    \-L|F7\n\
    \7S-7|\n\
    \L|7||\n\
    \-L-J|\n\
    \L|-JF\n"

testInput3 :: String
testInput3 =
    "\
    \..F7.\n\
    \.FJ|.\n\
    \SJ.L7\n\
    \|F--J\n\
    \LJ...\n"

testInput4 :: String
testInput4 =
    "\
    \7-F7-\n\
    \.FJ|7\n\
    \SJLL7\n\
    \|F--J\n\
    \LJ.LJ\n"

partI :: IO ()
partI = do
    -- grid <- lines <$> readFile "data/2023/day10-3-test.txt"
    grid <- lines <$> return testInput4
    let s = startPoint grid
        ns = map (\(p, _, _) -> p) $ neibours s grid
        x = head ns
        y = ns !! 1
        state@(MazeState _ _ m' _ _) = startMazeState grid s x y
    -- print m'
    (m, l) <- evalStateT (bfs [x]) state
    print m
    let g' = fillGrid grid m
    -- forM_ (groupBy (\a b -> snd a == snd b) $ M.toList m) print
    -- print s
    drawGrid g'
    print ((l + 1) `div` 2)

partII = do
    grid <- lines <$> readFile "data/2023/day10.txt"
    -- grid <- lines <$> return testInput4
    let s = startPoint grid
        ns = map (\(p, _, _) -> p) $ neibours s grid
        x = head ns
        y = ns !! 1
        state@(MazeState _ _ m' _ _) = startMazeState grid s x y
    -- print m'
    (m, _) <- evalStateT (bfs [x]) state
    let paths = getPoints m
        results = part2 paths
    -- forM_ (groupBy (\a b -> snd a == snd b) $ M.toList m) print
    -- print s
    print results

getPoints :: Map Point Int -> [Point]
getPoints = map fst . sortOn snd . M.toList

-- pickT :: Int -> Int -> Int
-- pickT area b = ( ( abs area )+ 1) - (b `div` 2)

-- shoeLace :: [Point] -> Int
-- shoeLace [_] = 0
-- shoeLace ((x1, y1) : (x2, y2) : xs) = (x1*y2 - x2*y1 + shoeLace ((x2, y2) : xs)) `div` 2
shoeLace :: [(Int, Int)] -> Int
shoeLace [_] = 0
shoeLace ((x1, y1) : (x2, y2) : xs) = (y1 + y2) * (x1 - x2) + shoeLace ((x2, y2) : xs)

---  Pick's Theorem
part2 :: [Point] -> Int
part2 path = (abs (shoeLace path) - length path + 2) `div` 2
