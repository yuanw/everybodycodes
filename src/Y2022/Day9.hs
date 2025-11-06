module Y2022.Day9 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (
    Parser,
    digit,
    endOfLine,
    parseOnly,
    string,
 )

import Data.Text qualified as T

import Data.Set qualified as Set

import Data.Either (fromRight)

type Point = (Int, Int)

-- head, tail
type RopeState = (Point, Point)
type Rope = [Point]

distant :: Point -> Point -> Int
distant (hx, hy) (tx, ty) = abs (hx - tx) `max` abs (hy - ty)

follow :: Point -> Point -> Point
follow h t = if distant h t < 2 then t else helper h t
  where
    helper :: Point -> Point -> Point
    helper (hx, hy) (tx, ty) = case (compare hx tx, compare hy ty) of
        (EQ, GT) -> (tx, ty + 1)
        (EQ, LT) -> (tx, ty - 1)
        (LT, EQ) -> (tx - 1, ty)
        (GT, EQ) -> (tx + 1, ty)
        (GT, GT) -> (tx + 1, ty + 1)
        (LT, LT) -> (tx - 1, ty - 1)
        (GT, LT) -> (tx + 1, ty - 1)
        (LT, GT) -> (tx - 1, ty + 1)
        (_, _) -> error "incorrect comb"

moveRope :: Rope -> Rope
moveRope [] = []
moveRope [p] = [p]
moveRope (x : y : xs) = if distant x y < 2 then (x : y : xs) else let y' = follow x y in x : moveRope (y' : xs)

data Movement = R Int | U Int | L Int | D Int deriving stock (Show)

parserMovement :: Parser Movement
parserMovement = (string "R " >> R . read <$> many digit) <|> (string "U " >> U . read <$> many digit) <|> (string "L " >> L . read <$> many digit) <|> (string "D " >> D . read <$> many digit)

movementParser :: Parser [Movement]
movementParser = many (parserMovement <* endOfLine)

move :: Movement -> Rope -> [Rope]
move (R 0) r = [r]
move (R n) r =
    let p = head r
        (hx, hy) = p
        r' = moveRope ((hx, hy + 1) : tail r)
     in r' : move (R (n - 1)) r'
move (U 0) r = [r]
move (U n) r =
    let p = head r
        (hx, hy) = p
        r' = moveRope ((hx + 1, hy) : tail r)
     in r' : move (U (n - 1)) r'
move (L 0) r = [r]
move (L n) r =
    let p = head r
        (hx, hy) = p
        r' = moveRope ((hx, hy - 1) : tail r)
     in r' : move (L (n - 1)) r'
move (D 0) r = [r]
move (D n) r =
    let p = head r
        (hx, hy) = p
        r' = moveRope ((hx - 1, hy) : tail r)
     in r' : move (D (n - 1)) r'
empty :: Rope
empty = [(0, 0), (0, 0)]

empty' :: Rope
empty' = [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]

visited :: Rope -> [Movement] -> (Rope, [Point])
visited r =
    foldl
        ( \(s, points) m ->
            let ns = move m s
                points' = map last ns
             in (last ns, points ++ points')
        )
        (r, [(0, 0)])

-- ..##..
-- ...##.
-- .####.
-- ....#.
-- s###..
test :: IO ()
test = do
    input <- readFile "data/2022/day9-test.txt"
    let movements = fromRight [] $ parseOnly movementParser $ T.pack input
        (r, points) =
            visited
                empty'
                [ R 5
                , U 8
                , L 8
                , D 3
                , R 17
                , D 10
                , L 25
                , U 20
                ]
    -- r = moveRope ((0,1): (tail empty'))
    -- print movements
    print r
    print (length r)
    print points
    print . Set.size . Set.fromList $ points

partI :: IO Int
partI = do
    input <- readFile "data/2022/day9.txt"
    let movements = fromRight [] $ parseOnly movementParser $ T.pack input
        -- (s, points) = visited [R 4 , U 4 , L 3, D 1, R 4, D 1, L 5, R 2]
        (_, points) = visited empty movements
    return . Set.size . Set.fromList $ points

partII :: IO Int
partII = do
    input <- readFile "data/2022/day9.txt"
    let movements = fromRight [] $ parseOnly movementParser $ T.pack input
        (_, points) = visited empty' movements
    return . Set.size . Set.fromList $ points
