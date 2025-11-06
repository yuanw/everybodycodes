module Y2022.Day10 where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (
    Parser,
    digit,
    endOfLine,
    many1,
    parseOnly,
    string,
 )
import Data.Either (fromRight)
import Data.List.Split (chunksOf)
import Data.Text qualified as T

data Instruction = Noop | AddX Int deriving stock (Show)

instructionParser :: Parser Instruction
instructionParser = (string "noop" >> return Noop) <|> (string "addx " >> AddX . read <$> many1 digit) <|> (string "addx -" >> AddX . (0 -) . read <$> many1 digit)

parseInstructions :: Parser [Instruction]
parseInstructions = many (instructionParser <* endOfLine)

nthCycle :: Int -> [Instruction] -> Int
nthCycle n instructions = let r = if n >= length results then results else take n results in n * last r
  where
    results = eva instructions

eva :: [Instruction] -> [Int]
eva =
    foldl
        ( \results instruction ->
            let result = last results
             in case instruction of
                    Noop -> results ++ [result]
                    AddX n -> results ++ [result, result + n]
        )
        [1]

partI :: IO Int
partI = do
    input <- readFile "data/2022/day10.txt"
    let instructions = fromRight [] $ parseOnly parseInstructions $ T.pack input
    return $ nthCycle 20 instructions + nthCycle 60 instructions + nthCycle 100 instructions + nthCycle 140 instructions + nthCycle 180 instructions + nthCycle 220 instructions

test :: IO ()
test = do
    input <- readFile "data/2022/day10.txt"
    let instructions = fromRight [] $ parseOnly parseInstructions $ T.pack input
        -- r = nthCycle 5 [Noop, AddX 3, AddX (-5)]
        s = length (eva instructions)
        art = [drawPixel i instructions | i <- [1 .. 240]]
    mapM_ print (chunksOf 40 art)

-- print $ nthCycle 20 instructions
-- print $ nthCycle 60 instructions
-- print $ nthCycle 100 instructions
-- print $ nthCycle 140 instructions
-- print $ nthCycle 180 instructions
-- print $ nthCycle 220 instructions
-- print $ nthCycle 20 instructions + nthCycle 60 instructions + nthCycle 100 instructions + nthCycle 140 instructions + nthCycle 180 instructions + nthCycle 220 instructions
-- r = moveRope ((0,1): (tail empty'))
-- print movements

width :: Int
width = 40

visible :: Int -> [Instruction] -> Bool
visible n instructions = let w = last (take n (eva instructions)) in abs ((n - 1) `mod` width - w) <= 1

drawPixel :: Int -> [Instruction] -> Char
drawPixel n instructions = if visible n instructions then '#' else '.'
