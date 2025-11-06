{-# LANGUAGE PatternGuards #-}

module Y2021.Day2 (day2PartI, day2PartII) where

import Data.Functor.Compose (Compose (getCompose))
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)

data Command = Forward Int | Down Int | Up Int deriving (Show)

data Position = Position Int Int

-- https://stackoverflow.com/questions/1602243/pattern-matching-string-prefixes-in-haskell
-- PatternGurads
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/pattern-and-guard-extensions#patternguards
readCommand :: String -> Maybe Command
readCommand xs
    | Just rest <- stripPrefix "forward " xs =
        (Just . Forward . read) rest
    | Just rest <- stripPrefix "down" xs =
        (Just . Down . read) rest
    | Just rest <- stripPrefix "up" xs =
        (Just . Up . read) rest
    | otherwise = Nothing

getCommands :: IO [Command]
getCommands = mapMaybe readCommand . lines <$> readFile "data/2021/day2.txt"

calculate :: [Command] -> Int
calculate = m . foldl move (Position 0 0)
  where
    m :: Position -> Int
    m (Position x y) = x * y
    move :: Position -> Command -> Position
    move (Position x y) c = case c of
        Forward x' -> Position (x + x') y
        Down y' -> Position x (y + y')
        Up y' -> Position x (y - y')

testCommands :: [Command]
testCommands =
    [ Forward 5
    , Down 5
    , Forward 8
    , Up 3
    , Down 8
    , Forward 2
    ]

day2PartI :: IO ()
day2PartI = do
    commands <- getCommands
    print $ calculate commands

data Position' = Position' Int Int Int deriving (Show)

calculate' :: [Command] -> Int
calculate' = m . foldl move (Position' 0 0 0)
  where
    m :: Position' -> Int
    m (Position' x y _) = x * y
    move :: Position' -> Command -> Position'
    move (Position' x y a) c = case c of
        Forward x' -> Position' (x + x') (y + x' * a) a
        Down a' -> Position' x y (a + a')
        Up a' -> Position' x y (a - a')

day2PartII :: IO ()
day2PartII = do
    commands <- getCommands
    print $ calculate' commands
