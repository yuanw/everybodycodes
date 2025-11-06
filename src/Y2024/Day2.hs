module Y2024.Day2 where

data Direction = Inc | Dec | Unknown deriving (Show, Eq)

data Seq = Seq (Maybe Int) Direction Bool deriving (Show, Eq)

badSeq :: Seq
badSeq = Seq Nothing Unknown False

lift :: Int -> Seq
lift num = Seq (Just num) Unknown True

union :: Seq -> Seq -> Seq
union (Seq _ _ False) Seq{} = badSeq
union Seq{} (Seq _ _ False) = Seq Nothing Unknown False
union (Seq _ Inc _) (Seq _ Dec _) = Seq Nothing Unknown False
union (Seq _ Dec _) (Seq _ Inc _) = badSeq
union (Seq Nothing Unknown True) other = other
union other (Seq Nothing Unknown True) = other
union (Seq (Just a) Unknown True) (Seq (Just b) Unknown True) = if a > b && b + 4 > a then (Seq (Just b) Dec True) else (if b > a && a + 4 > b then (Seq (Just b) Inc True) else badSeq)
union (Seq (Just a) Inc True) (Seq (Just b) _ _) = if b > a && a + 4 > b then (Seq (Just b) Inc True) else badSeq
union (Seq (Just a) Dec True) (Seq (Just b) _ _) = if a > b && b + 4 > a then (Seq (Just b) Dec True) else badSeq
union _ _ = badSeq

instance Semigroup Seq where
    (<>) = union

instance Monoid Seq where
    mempty = Seq Nothing Unknown True

isSafe :: Seq -> Bool
isSafe (Seq _ _ s) = s

process :: [String] -> Seq
process = foldl (\seq' num -> seq' <> (lift . readInt $ num)) (Seq Nothing Unknown True)
  where
    readInt :: String -> Int
    readInt = read

process' :: [String] -> Bool
process' xs = (isSafe (process xs)) || (any isSafe [(process ys) | ys <- rest xs])

partI :: IO ()
partI = do
    inputs <- map words . lines <$> readFile "data/2024/day2.txt"
    print $ length . filter isSafe $ map process inputs

partII :: IO ()
partII = do
    inputs <- map words . lines <$> readFile "data/2024/day2.txt"
    print $ length . filter id $ map process' inputs

rest :: [String] -> [[String]]
rest xs = map (map snd) [removeItem i (zip [0 ..] xs) | i <- [0 .. (length xs)]]
  where
    removeItem :: Int -> [(Int, String)] -> [(Int, String)]
    removeItem _ [] = []
    removeItem x ((index, y) : ys)
        | x == index = ys
        | otherwise = (index, y) : removeItem x ys
