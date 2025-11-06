{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day2 (partI, partII) where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as TIO

data Color = Red | Blue | Green deriving (Show, Eq, Ord)

data Config = Config {totalBlueCount :: Int, totalRedCount :: Int, totalGreenCount :: Int} deriving (Show, Eq)

data Game = Game {gameId :: Int, bags :: [Config]} deriving (Show)

data Cube = Cube Int Color deriving (Show)

possible :: Config -> Game -> Bool
possible c (Game _ ba) = foldr (\a b -> b && help c a) True ba
  where
    help (Config lb lr lg) (Config rb rr rg) = lb >= rb && lr >= rr && lg >= rg

cubeParser :: Parser Cube
cubeParser = do
    char ' '
    i <- decimal
    char ' '
    color <- (string "blue" >> return Blue) <|> (string "red" >> return Red) <|> (string "green" >> return Green)
    return $ Cube i color

bagsParser :: Parser Config
bagsParser = do
    bags <- cubeParser `sepBy` char ','
    return $ toConfig (foldCube bags)
  where
    foldCube :: [Cube] -> Map.Map Color Int
    foldCube = foldr (\(Cube i c) m -> Map.insertWith (+) c i m) Map.empty

    toConfig :: Map.Map Color Int -> Config
    toConfig m = Config (fromMaybe 0 $ Map.lookup Blue m) (fromMaybe 0 $ Map.lookup Red m) (fromMaybe 0 $ Map.lookup Green m)

gameParser :: Parser Game
gameParser = do
    _ <- string "Game "
    i <- decimal
    _ <- char ':'
    cs <- bagsParser `sepBy` char ';'
    return $ Game i cs

gamesParser :: Parser [Game]
gamesParser = many $ gameParser <* endOfLine

powerOfMinSet :: Game -> Int
powerOfMinSet g = let bs = bags g in minRed bs * minBlue bs * minGreen bs
  where
    minRed = maximum . map totalRedCount
    minBlue = maximum . map totalBlueCount
    minGreen = maximum . map totalGreenCount

partI :: IO ()
partI = do
    input <- TIO.readFile "data/2023/day2.txt"
    let games = fromRight [] (parseOnly gamesParser input)
        config = Config 14 12 13
        possibleGames = filter (possible config) games
    print (sum $ map gameId possibleGames)

partII :: IO ()
partII = do
    input <- TIO.readFile "data/2023/day2.txt"
    let games = fromRight [] (parseOnly gamesParser input)
    print (sum $ map powerOfMinSet games)
