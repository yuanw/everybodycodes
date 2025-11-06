{-# LANGUAGE OverloadedStrings #-}

module Y2024.Day3 where

import Control.Applicative
import Data.Traversable
import Data.Void (Void)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as PL

type CharParser = P.Parsec Void String

eitherToMaybe :: (Alternative m) => Either e a -> m a
eitherToMaybe = either (const empty) pure

parseMaybeLenient :: P.Parsec Void s a -> s -> Maybe a
parseMaybeLenient p = eitherToMaybe . P.parse p "parseMaybeLenient"

pDropUntil :: (P.Stream s, Ord e) => P.Parsec e s end -> P.Parsec e s end
pDropUntil = P.try . P.skipManyTill P.anySingle . P.try

-- | Alias for 'parseMaybeLenient'
parseMaybe' :: P.Parsec Void s a -> s -> Maybe a
parseMaybe' = parseMaybeLenient

parseMul :: CharParser Int
parseMul = product <$> P.between "mul(" ")" (replicate 2 PL.decimal `sequenceSepBy` ",")

sequenceSepBy ::
    (Traversable t, P.Stream s, Ord e) => t (P.Parsec e s a) -> P.Parsec e s sep -> P.Parsec e s (t a)
sequenceSepBy xs sep = sequenceA . snd $ mapAccumR go False xs
  where
    go addSep x = (True, if addSep then x' <* sep else x')
      where
        x' = P.notFollowedBy sep *> P.try x

doOrDoNot :: CharParser Int
doOrDoNot = sum <$> goEnabled
  where
    goDisabled :: CharParser [Int]
    goDisabled = P.option [] . pDropUntil $ "do()" *> goEnabled
    goEnabled :: CharParser [Int]
    goEnabled =
        P.option [] . pDropUntil . P.choice $
            [ "don't()" *> goDisabled
            , (:) <$> parseMul <*> goEnabled
            ]

partI :: IO ()
partI = do
    test <- readFile "data/2024/day3.txt"
    --   print $ (parseMaybe' $ sum <$> many pDropUntil parseMul) test
    P.parseTest (sum <$> many (pDropUntil parseMul)) test

partII :: IO ()
partII = do
    test <- readFile "data/2024/day3.txt"
    --   print $ (parseMaybe' $ sum <$> many pDropUntil parseMul) test
    P.parseTest doOrDoNot test
