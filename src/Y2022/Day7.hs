{-# LANGUAGE OverloadedStrings #-}

module Y2022.Day7 where

import Control.Applicative (many, (<|>))
import Control.Monad (join)
import Data.Attoparsec.Text (
    Parser,
    char,
    digit,
    endOfLine,
    letter,
    many1,
    parseOnly,
    space,
    string,
 )
import Data.Either (fromRight)
import Data.Text qualified as T

data FileSystem = Directory {_dName :: String, _content :: [FileSystem]} | File {_fName :: String, _size :: Int} deriving stock (Show)

getSize :: FileSystem -> Int
getSize (File _ s) = s
getSize (Directory _ []) = 0
getSize (Directory n (x : xs)) = getSize x + getSize (Directory n xs)
type Name = String
type Size = Int
data Output = CD String | LS | Dir String | FL Int String deriving stock (Show)

data FSCrumb = FSCrumb {_parent :: Name, _before :: [FileSystem], _after :: [FileSystem]} deriving stock (Show)
type Zipper = (FileSystem, [FSCrumb])

parserFile :: Parser Output
parserFile = do
    size <- many1 digit
    _ <- space
    name <- many1 (letter <|> char '.' <|> digit)
    return $ FL (read size) name

outputParser :: Parser Output
-- outputParser = (string "$ ls" >> return LS) <|> (string "$ cd " >>  CD <$> manyTill anyChar (string "\n") )  <|> (string "dir " >>  Dir <$> manyTill anyChar (string "\n") ) <|> parserFile
outputParser = (string "$ ls" >> return LS) <|> (string "$ cd " >> CD <$> many (letter <|> char '/' <|> char '.')) <|> (string "dir " >> Dir <$> many (letter <|> digit)) <|> parserFile

outputsParser :: Parser [Output]
outputsParser = many (outputParser <* endOfLine)

empty :: Zipper
empty = (Directory "/" [], [])

test :: IO ()
test = do
    input <- readFile "data/2022/day7.txt"
    let outputs = fromRight [] $ parseOnly outputsParser $ T.pack input
        tree = foldl buildTree empty (tail outputs)
        root = topMost tree
        f = freeSpace root
    print f
    -- this feel so dirty
    print . minimum $ [s | d <- getDeFolder root, let s = getSize d, s >= f]

freeSpace :: FileSystem -> Int
freeSpace = (30000000 -) . (70000000 -) . getSize

getDeFolder :: FileSystem -> [FileSystem]
getDeFolder (File _ _) = []
getDeFolder d@(Directory _ cs) = d : join [getDeFolder c | c <- cs]

buildTree :: Zipper -> Output -> Zipper
buildTree z LS = z
buildTree (Directory name items, bs) (Dir folderName) = (Directory name $ items ++ [Directory folderName []], bs)
buildTree (Directory name items, bs) (FL size fileName) = (Directory name $ items ++ [File fileName size], bs)
buildTree z (CD "..") = goUp z
buildTree z (CD name) = goTo name z
buildTree _ _ = error ""

goTo :: String -> Zipper -> Zipper
goTo name (Directory folderName items, bs) = let (ls', rs') = break (nameIs name) items in (head rs', FSCrumb folderName ls' (tail rs') : bs)
goTo _ _ = error "cannot go file"

goUp :: Zipper -> Zipper
goUp (item, FSCrumb name ls rs : bs) = (Directory name (ls ++ item : rs), bs)
goUp _ = error ""

nameIs :: String -> FileSystem -> Bool
nameIs name (Directory folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

topMost :: Zipper -> FileSystem
topMost (item, []) = item
topMost z = topMost (goUp z)

isFolder :: FileSystem -> Bool
isFolder (Directory _ _) = True
isFolder _ = False

getSubFolder :: FileSystem -> [FileSystem]
getSubFolder (File _ _) = []
getSubFolder (Directory _ children) = [i | i <- children, isFolder i]

atMost :: Int -> FileSystem -> Int
atMost _ (File _ _) = 0
atMost limit d = (if getSize d <= limit then getSize d else 0) + sum (map (atMost limit) (getSubFolder d))
