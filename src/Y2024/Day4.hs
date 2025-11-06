module Y2024.Day4 where

type Grid = [String]

data Direction = North | NorthEast | East | EastSouth | South | SouthWest | West | NorthWest deriving (Show, Enum)

type Point = (Int, Int)

findCharByPoint :: Point -> Grid -> Maybe Char
findCharByPoint (x, y) grid = if x < 0 || (x + 1) > cols || y < 0 || (y + 1) > rows then Nothing else Just ((grid !! y) !! x)
  where
    cols = length grid
    rows = length (head grid)

move :: Point -> Direction -> Point
move (x, y) North = (x, y - 1)
move (x, y) NorthEast = (x + 1, y - 1)
move (x, y) East = (x + 1, y)
move (x, y) EastSouth = (x + 1, y + 1)
move (x, y) South = (x, y + 1)
move (x, y) SouthWest = (x - 1, y + 1)
move (x, y) West = (x - 1, y)
move (x, y) NorthWest = (x - 1, y - 1)

isXMas :: Point -> Grid -> Direction -> Bool
isXMas x grid direction = (findCharByPoint x grid == Just 'X') && (findCharByPoint m grid == Just 'M') && (findCharByPoint a grid == Just 'A') && (findCharByPoint s grid == Just 'S')
  where
    m = move x direction
    a = move m direction
    s = move a direction

isCrossMAS :: Point -> Grid -> Bool
isCrossMAS a grid =
    (nw == Just 'M' && sw == Just 'M' && ne == Just 'S' && es == Just 'S')
        || (nw == Just 'S' && sw == Just 'M' && ne == Just 'S' && es == Just 'M')
        || (nw == Just 'S' && sw == Just 'S' && ne == Just 'M' && es == Just 'M')
        || (nw == Just 'M' && sw == Just 'M' && ne == Just 'S' && es == Just 'S')
        || (nw == Just 'M' && sw == Just 'S' && ne == Just 'M' && es == Just 'S')
  where
    nw = findCharByPoint (move a NorthWest) grid
    sw = findCharByPoint (move a SouthWest) grid
    ne = findCharByPoint (move a NorthEast) grid
    es = findCharByPoint (move a EastSouth) grid

findAll :: Char -> Grid -> [Point]
findAll c grid = [(i, j) | i <- [0 .. cols], j <- [0 .. rows], findCharByPoint (i, j) grid == Just c]
  where
    cols = length grid
    rows = length (head grid)

count :: Grid -> [(Point, Direction)]
count grid = [(x, dir) | x <- xs, dir <- enumFrom North, isXMas x grid dir]
  where
    xs = findAll 'X' grid

count' :: Grid -> [Point]
count' grid = [a | a <- as, isCrossMAS a grid]
  where
    as = findAll 'A' grid

partI :: IO ()
partI = do
    grid <- lines <$> readFile "data/2024/day4.txt"
    print (length $ count grid)

partII :: IO ()
partII = do
    grid <- lines <$> readFile "data/2024/day4.txt"
    print (length $ count' grid)
    grid' <- lines <$> readFile "data/2024/test-day4.txt"
    print (length $ count' grid')
