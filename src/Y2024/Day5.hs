module Y2024.Day5 where

import Data.Containers.ListUtils (nubOrd)

import Data.Graph.Inductive qualified as G

import Data.Set qualified as S
import Data.Traversable
import Data.Void (Void)

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as PL

type CharParser = P.Parsec Void String

type Input = ([(Int, Int)], [[Int]])

-- | 'sepEndBy' but automatically exclude the separator from the internal parser
sepEndBy' :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s sep -> P.Parsec e s [a]
sepEndBy' x sep = P.sepEndBy (P.notFollowedBy sep *> P.try x) sep

sepEndByLines :: (P.Stream s, Ord e, P.Token s ~ Char) => P.Parsec e s a -> P.Parsec e s [a]
sepEndByLines = flip sepEndBy' P.newline

pDecimal :: forall a e s. (P.Stream s, P.Token s ~ Char, Ord e, Num a) => P.Parsec e s a
pDecimal = pTok $ PL.signed P.space PL.decimal

pTok :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s a -> P.Parsec e s a
pTok p = pSpace *> p <* pSpace

pSpace :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s ()
pSpace = P.skipMany (P.char ' ')

sepByLines :: (P.Stream s, Ord e, P.Token s ~ Char) => P.Parsec e s a -> P.Parsec e s [a]
sepByLines = flip sepBy' P.newline

-- | 'sepBy' but automatically exclude the separator from the internal parser
sepBy' :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s sep -> P.Parsec e s [a]
sepBy' x sep = P.sepBy (P.notFollowedBy sep *> P.try x) sep

sequenceSepBy ::
    (Traversable t, P.Stream s, Ord e) => t (P.Parsec e s a) -> P.Parsec e s sep -> P.Parsec e s (t a)
sequenceSepBy xs sep = sequenceA . snd $ mapAccumR go False xs
  where
    go addSep x = (True, if addSep then x' <* sep else x')
      where
        x' = P.notFollowedBy sep *> P.try x

parseRule :: CharParser (Int, Int)
parseRule = do
    a <- pDecimal
    P.char '|'
    b <- pDecimal
    pure (a, b)

parseInput :: CharParser ([(Int, Int)], [[Int]])
parseInput = do
    rules <- sepEndByLines parseRule
    _ <- P.newline
    pages <- sepByLines $ pDecimal `sepBy'` ","
    pure (rules, pages)

sorts :: ([(Int, Int)], [[Int]]) -> Int
sorts (rules, pages) = sum [maybe 0 id (findMid page) | page <- pages, (sortByRules rules page) == page]

sorts' :: ([(Int, Int)], [[Int]]) -> Int
sorts' (rules, pages) = sum [maybe 0 id (findMid sorted) | page <- pages, let sorted = sortByRules rules page, sorted /= page]

findMid :: [a] -> Maybe a
findMid [] = Nothing
findMid xs = let i = (length xs) `div` 2 in Just (xs !! i)

sortByRules :: [(Int, Int)] -> [Int] -> [Int]
sortByRules rules = \xs ->
    G.topsort . G.nfilter (`S.member` S.fromList xs) $ rulesGraph
  where
    rulesGraph :: G.Gr () ()
    rulesGraph =
        G.mkUGraph (nubOrd $ foldMap (\(a, b) -> [a, b]) rules) rules

partI :: IO ()
partI = do
    test <- readFile "data/2024/day5.txt"
    P.parseTest (sorts <$> parseInput) test

partII :: IO ()
partII = do
    test <- readFile "data/2024/day5.txt"
    P.parseTest (sorts' <$> parseInput) test
