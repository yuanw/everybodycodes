{-# LANGUAGE DerivingStrategies #-}

module Y2024.Day6 where
import Data.Set qualified as S

data Direction = West | North | East | South deriving stock (Show, Enum)

type Grid = [String]
type Point = (Int, Int)
type Guard = (Point, Direction)

possibleGuard :: String
possibleGuard = "<^>v"

obstacle :: Char
obstacle = '#'

data State = State Grid Guard (S.Set Point) Bool


move :: State -> State
move s = s




