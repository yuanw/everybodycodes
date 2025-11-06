{-# LANGUAGE OverloadedStrings #-}

module Y2022.Day5 (partI, partII) where

import Control.Applicative (many, (<|>))
import Control.Lens (element, (&), (.~))
import Data.Attoparsec.Text hiding (take)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.List.Split (splitOn)
import Data.Text qualified as T

type Crate = Char
type Stack = [Crate]
type Stacks = [Stack]
data Procedure = Procedure Int Int Int deriving stock (Show)

splitStack :: String -> String
splitStack = head . splitOn "\n\n"

parseStack :: String -> Stacks
parseStack = snd . foldr parse' ([], []) . lines
  where
    parse' :: String -> ([Int], [Stack]) -> ([Int], [Stack])
    parse' input (indices, rows) = if null indices then parseHeader input else parseRow input (indices, rows)

    parseHeader :: String -> ([Int], Stacks)
    parseHeader input = (indices, stacks)
      where
        indices = foldr (\(index, x) y -> if isSpace x then y else index : y) [] (zip [0 ..] input)
        stacks = [[] | _ <- indices]
    parseRow :: String -> ([Int], [Stack]) -> ([Int], [Stack])
    parseRow input (indices, rows) = (indices, newRows)
      where
        row = [lookUp input index | index <- indices]

        lookUp :: String -> Int -> Maybe Crate
        lookUp x i = if i < length x then Just (x !! i) >>= (\a -> if isSpace a then Nothing else Just a) else Nothing

        -- newRows = map (\(x, y) -> maybe y (: y) x) (zip row rows)
        newRows = zipWith (\x y -> maybe y (: y) x) row rows

procedureParse :: Parser Procedure
procedureParse = do
    _ <- string "move"
    _ <- space
    count' <- count 2 digit <|> count 1 digit
    _ <- space
    _ <- string "from"
    _ <- space
    from <- count 1 digit
    _ <- space
    _ <- string "to"
    _ <- space
    to <- count 1 digit
    return $ Procedure (read count') (read from) (read to)

proceduresParser :: Parser [Procedure]
proceduresParser = many $ procedureParse <* endOfLine

test :: IO ()
test = do
    input <- readFile "data/2022/day5.txt"
    let stack = parseStack (head . splitOn "\n\n" $ input)
        procedures = fromRight [] $ parseOnly proceduresParser (T.pack . last . splitOn "\n\n" $ input)
    -- print stack
    -- print procedures
    -- print $ foldl move stack procedures
    print . fmap head $ foldl move' stack procedures

solution :: (Stacks -> Procedure -> Stacks) -> IO ()
solution fn = do
    input <- readFile "data/2022/day5.txt"
    let stack = parseStack (head . splitOn "\n\n" $ input)
        procedures = fromRight [] $ parseOnly proceduresParser (T.pack . last . splitOn "\n\n" $ input)
    -- print stack
    -- print procedures
    -- print $ foldl move stack procedures
    print . fmap head $ foldl fn stack procedures

partI :: IO ()
partI = solution move

partII :: IO ()
partII = solution move'

move :: Stacks -> Procedure -> Stacks
move stacks (Procedure 0 _ _) = stacks
move stacks (Procedure n f t) = move (stacks & element (f - 1) .~ newFrom & element (t - 1) .~ newTo) (Procedure (n - 1) f t)
  where
    from = stacks !! (f - 1)
    to = stacks !! (t - 1)
    newFrom = tail from
    newTo = head from : to

move' :: Stacks -> Procedure -> Stacks
move' stacks (Procedure n f t) = stacks & element (f - 1) .~ newFrom & element (t - 1) .~ newTo
  where
    from = stacks !! (f - 1)
    to = stacks !! (t - 1)
    newFrom = drop n from
    newTo = take n from ++ to
