{-# LANGUAGE OverloadedStrings #-}

module Y2023.Day5 (partI', partII') where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text
import Data.Either (fromRight)
import Data.Interval (Interval)
import Data.Interval qualified as IV
import Data.IntervalMap.Strict (IntervalMap)
import Data.IntervalMap.Strict qualified as IVM
import Data.IntervalSet (IntervalSet)
import Data.IntervalSet qualified as IVS
import Data.List (foldl', nub)
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Text.IO qualified as TIO

type Seed = Int
type Seeds = [Seed]

data MapEntry = MapEntry Int Int Int deriving (Show)

data Input = Input Seeds [[MapEntry]] deriving (Show)

fromRange :: Int -> Int -> Interval Int
fromRange x len = IV.Finite x IV.<=..< IV.Finite (x + len)

-- | Takes in the (a, b, c) triples that the problem gives map chunks
buildMap :: [MapEntry] -> IntervalMap Int Int
buildMap = IVM.fromList . map (\(MapEntry dest src len) -> (fromRange src len, dest - src))

convertSingle :: Int -> IntervalMap Int Int -> Int
convertSingle x mp = case IVM.lookup x mp of
    Nothing -> x -- the value is not in any known interval
    Just delta -> x + delta -- that interval has the given delta

convertMany :: IntervalSet Int -> IntervalMap Int Int -> IntervalSet Int
convertMany xs mp = misses <> hits
  where
    -- dummy map corresponding to putting `()` at every interval in our
    -- `IntervalSet`, needed because interval map functions require maps
    tempMap :: IntervalMap Int ()
    tempMap = IVM.fromList . map (,()) . IVS.toList $ xs

    misses = IVM.keysSet $ tempMap `IVM.difference` mp
    hits =
        IVS.fromList
            . map (\(iv, delta) -> IV.mapMonotonic (+ delta) iv)
            . IVM.toList
            $ IVM.intersectionWith const mp tempMap

skipRestOfLine :: Parser ()
skipRestOfLine = skipWhile (not . isEndOfLine) >> endOfLine

seedsParser :: Parser Seeds
seedsParser = string "seeds: " *> decimal `sepBy` char ' '

mapEntryParser :: Parser MapEntry
mapEntryParser = do
    a <- decimal
    char ' '
    b <- decimal
    char ' '
    c <- decimal
    return $ MapEntry a b c

mapEntriesParser :: Parser [MapEntry]
mapEntriesParser = many $ mapEntryParser <* endOfLine

mapping :: Int -> MapEntry -> Maybe Int
mapping v (MapEntry dest source range) = if v >= source && v <= (source + range) then Just (v - source + dest) else Nothing

mappingCategory :: Int -> [MapEntry] -> Int
mappingCategory v entries = fromMaybe v $ foldl (\accum entry -> if isJust accum then accum else mapping v entry) Nothing entries

mappingCategories :: Int -> [[MapEntry]] -> Int
mappingCategories = foldl mappingCategory

range :: [Seed] -> [Int]
range [] = []
range (x : y : xs) = [x + i | i <- [0 .. y - 1]] ++ range xs

solve :: Input -> [Int]
solve (Input seeds entries) = map (\s -> mappingCategories s entries) seeds

solve' :: Input -> [Int]
solve' (Input seeds entries) = map (\s -> mappingCategories s entries) (nub $ range seeds)

inputParser :: Parser Input
inputParser = do
    seeds <- seedsParser
    endOfLine
    skipRestOfLine
    string "seed-to-soil map:"
    skipRestOfLine
    entries <- mapEntriesParser
    skipRestOfLine
    string "soil-to-fertilizer map:"
    skipRestOfLine
    s2f <- mapEntriesParser
    skipRestOfLine
    string "fertilizer-to-water map:"
    skipRestOfLine
    f2w <- mapEntriesParser
    skipRestOfLine
    string "water-to-light map:"
    skipRestOfLine
    w2l <- mapEntriesParser
    skipRestOfLine
    string "light-to-temperature map:"
    skipRestOfLine
    l2t <- mapEntriesParser
    skipRestOfLine
    string "temperature-to-humidity map:"
    skipRestOfLine
    t2h <- mapEntriesParser
    skipRestOfLine
    string "humidity-to-location map:"
    skipRestOfLine
    h2l <- mapEntriesParser
    return $ Input seeds [entries, s2f, f2w, w2l, l2t, t2h, h2l]

partI :: IO ()
partI = do
    rawInput <- TIO.readFile "data/2023/day5.txt"
    -- TIO.putStrLn input
    let e = (parseOnly inputParser rawInput)
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        -- Right logs ->  printDetails logs
        Right input@(Input _ entries) -> print (minimum $ solve input)
partII :: IO ()
partII = do
    rawInput <- TIO.readFile "data/2023/day5.txt"
    -- TIO.putStrLn input
    let e = (parseOnly inputParser rawInput)
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        -- Right logs ->  printDetails logs
        Right input@(Input seeds entries) -> print (minimum $ solve' input)

partI' :: IO ()
partI' = do
    rawInput <- TIO.readFile "data/2023/day5.txt"
    -- TIO.putStrLn input
    let e = (parseOnly inputParser rawInput)
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        -- Right logs ->  printDetails logs
        Right input@(Input seeds entries) -> print (minimum $ map (\s0 -> foldl' convertSingle s0 (map buildMap entries)) seeds)

prepare :: [Seed] -> [Interval Int]
prepare = fmap (uncurry fromRange) . catMaybes . fmap listTup . chunksOf 2

listTup :: [a] -> Maybe (a, a)
listTup (x : y : _) = Just (x, y)
listTup _ = Nothing

partII' :: IO ()
partII' = do
    rawInput <- TIO.readFile "data/2023/day5.txt"
    -- TIO.putStrLn input
    let e = (parseOnly inputParser rawInput)
    case e of
        Left err -> putStrLn $ "Error while parsing: " ++ err
        -- Right logs ->  printDetails logs
        Right input@(Input seeds entries) -> print (fromFinite . IV.lowerBound . IVS.span $ foldl' convertMany (IVS.fromList (prepare seeds)) (map buildMap entries))
  where
    fromFinite = \case
        IV.Finite x -> Just x
        _ -> Nothing
