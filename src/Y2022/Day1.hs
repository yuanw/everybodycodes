module Y2022.Day1 (partI, partII) where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down (Down))
import Data.Semigroup (Max (Max))

what :: FilePath -> IO [[Int]]
what path = map (map readNum . lines) . splitOn "\n\n" <$> readFile path
  where
    readNum :: String -> Int
    readNum = read

partI :: IO ()
partI = do
    a <- what "data/2022/day1.txt"
    print (foldMap (Max . sum) a)

partII :: IO ()
partII = do
    a <- what "data/2022/day1.txt"
    print $ (sum . foldThree . map sum) a

newtype TopThree n = TopThree {getTopThree :: (Maybe n, Maybe n, Maybe n)}

getFirst :: TopThree n -> Maybe n
getFirst three = x
  where
    (x, _, _) = getTopThree three

getSec :: TopThree n -> Maybe n
getSec three = x
  where
    (_, x, _) = getTopThree three

getThird :: TopThree n -> Maybe n
getThird three = x
  where
    (_, _, x) = getTopThree three

toList :: TopThree n -> [n]
toList t = catMaybes [getFirst t, getSec t, getThird t]

appendTopThree :: (Ord n, Num n) => TopThree n -> TopThree n -> TopThree n
appendTopThree l r = case nums of
    [] -> TopThree (Nothing, Nothing, Nothing)
    [a] -> TopThree (Just a, Nothing, Nothing)
    [a, b] -> TopThree (Just a, Just b, Nothing)
    -- [a, b, c] -> TopThree (Just a) (Just b) (Just c)
    (a : b : c : _) -> TopThree (Just a, Just b, Just c)
  where
    nums = sortOn Down (toList l ++ toList r)

instance (Ord n, Num n) => Semigroup (TopThree n) where
    (<>) = appendTopThree

instance (Ord n, Num n) => Monoid (TopThree n) where
    mempty = TopThree (Nothing, Nothing, Nothing)

foldThree :: (Ord n, Num n, Foldable t) => t n -> TopThree n
foldThree = foldl (\ac n -> wrap n <> ac) mempty
  where
    wrap :: a -> TopThree a
    wrap x = TopThree (Just x, Nothing, Nothing)

instance Functor TopThree where
    fmap f t = TopThree ((fmap f . getFirst) t, (fmap f . getSec) t, (fmap f . getThird) t)

instance Foldable TopThree where
    foldMap f t = maybe mempty f (getFirst t) <> maybe mempty f (getSec t) <> maybe mempty f (getThird t)

-- sumTopThree :: TopThree Int -> Int
-- sumTopThree = sum . toList
