{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day8 (partI, partII) where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

data Direction = L | R deriving (Show)

type Node = String
newtype Path = Path {getPath :: (Node, Node)} deriving (Show)

type Network = Map Node Path
type Directions = [Direction]
type Input = (Directions, Network)

data World a = World {getCurrentNode :: a, getDirections :: Directions, getNetwork :: Map Node Path, getStep :: Int} deriving (Show)

-- data World' = World' {getCurrentNodes :: [Node], getDirections' :: Directions, getNetwork' :: Map Node Path, getStep' :: Int} deriving (Show)

directionParser :: Parser Direction
directionParser = (char 'R' >> return R) <|> (char 'L' >> return L)

directionsParser :: Parser Directions
directionsParser = many directionParser

nodeParser :: Parser (Node, Path)
nodeParser = do
    node <- takeTill (== ' ')
    skipSpace
    _ <- char '='
    skipSpace
    _ <- char '('
    l <- takeTill (== ',')
    _ <- char ','
    skipSpace
    r <- takeTill (== ')')
    _ <- char ')'
    return (T.unpack node, Path (T.unpack l, T.unpack r))

nodesParser :: Parser [(Node, Path)]
nodesParser = many $ nodeParser <* endOfLine

inputParser :: Parser Input
inputParser = do
    ds <- directionsParser
    endOfLine
    endOfLine
    nodes <- nodesParser
    return (ds, M.fromList nodes)

start :: Input -> a -> World a
start (ds, network) p = World p ds network 0

runI :: Input -> World Node
runI input = h (start input "AAA")
  where
    h :: World Node -> World Node
    h w = if (reachEnd w) then w else (h (next w))

    reachEnd :: World Node -> Bool
    reachEnd = (== "ZZZ") . getCurrentNode

    next :: World Node -> World Node
    next (World c ds ns s) = World (f currentNode d) ds ns (s + 1)
      where
        d = ds !! (s `mod` length ds)
        currentNode = getPath $ fromJust (M.lookup c ns) :: (Node, Node)
        f :: (Node, Node) -> Direction -> Node
        f (l, _) L = l
        f (_, r) R = r

runII :: Input -> Int
runII input = foldl lcm 1 $ map (getStep . h . start input) (filter ((== 'A') . last) $ M.keys network)
  where
    (_, network) = input

    h :: World Node -> World Node
    h w = if reachEnd w then w else h (next w)
    reachEnd :: World Node -> Bool
    reachEnd = ((== 'Z') . last) . getCurrentNode

    next :: World Node -> World Node
    next (World c ds ns s) = World (f currentNode d) ds ns (s + 1)
      where
        d = ds !! (s `mod` length ds)
        currentNode = getPath $ fromJust (M.lookup c ns) :: (Node, Node)
        f :: (Node, Node) -> Direction -> Node
        f (l, _) L = l
        f (_, r) R = r

-- lcm :: Int -> Int -> Int
-- lcm a b = (a*b) `div` gcd a b
--   -- where gcd :: Int -> Int -> Int
--   --       gcd = undefined
partI :: IO ()
partI = do
    rawInput <- TIO.readFile "data/2023/day8.txt"
    let e = parseOnly inputParser rawInput
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right input -> print (getStep $ runI input)

partII :: IO ()
partII = do
    rawInput <- TIO.readFile "data/2023/day8.txt"
    let e = parseOnly inputParser rawInput
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        Right input -> print (runII input)
