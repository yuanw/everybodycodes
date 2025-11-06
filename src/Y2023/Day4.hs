{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day4 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text.IO qualified as TIO

data Card = Card {cardId :: Int, winNo :: [Int], myNo :: [Int]} deriving (Show)
type Pile = [Card]

cardParser :: Parser Card
cardParser = do
    _ <- string "Card"
    skipSpace
    i <- decimal
    _ <- string ":"
    skipSpace
    wN <- many1 (decimal <* skipSpace)
    skipSpace
    _ <- string "|"
    skipSpace
    mN <- many1 (decimal <* skipSpace)
    return $ Card i wN mN

cardsParser :: Parser Pile
cardsParser = many cardParser

point :: Card -> Int
point (Card _ wN mN) = foldr (\n s -> if n `elem` wN then s + 1 else s) 0 mN

scoreI :: Int -> Int
scoreI 0 = 0
scoreI n = 2 ^ (n - 1)

zero :: Pile -> Map.Map Int Int
zero = Map.fromList . (\c -> [(i, 1) | i <- [1 .. c]]) . cardId . last

scoreII :: Pile -> Int
scoreII pile = sum . Map.elems $ foldl (\m card -> updateM (cardId card) (point card) m) (zero pile) pile
  where
    updateM :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
    updateM cid _point oldM = if _point == 0 then oldM else foldl (\m k -> Map.insertWith (+) k (fromJust $ Map.lookup cid oldM) m) oldM [cid + 1 .. cid + _point]

partI :: IO ()
partI = do
    input <- TIO.readFile "data/2023/day4-2-test.txt"
    -- TIO.putStrLn input
    let cards = fromRight [] (parseOnly cardsParser input)
        scores = sum $ map (scoreI . point) cards
    print scores

partII :: IO ()
partII = do
    input <- TIO.readFile "data/2023/day4.txt"
    -- TIO.putStrLn input
    let cards = fromRight [] (parseOnly cardsParser input)
        scores = scoreII cards
    print scores
