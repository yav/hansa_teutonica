module Stats where

import Basics

data Stat = Keys | Actions | Privilege | Movement | Hire
  deriving (Eq,Ord,Show,Bounded,Enum)

type Level = Int

maxStat :: Stat -> Level
maxStat s =
  case s of
    Keys      -> 5
    Actions   -> 6
    _         -> 4

statWorker :: Stat -> WorkerType
statWorker s =
  case s of
    Movement -> Disc
    _        -> Cube

maxStatePoints :: Stat -> Int
maxStatePoints s =
  case s of
    Keys -> 0
    _    -> 4

keyPoints :: Level -> Int
keyPoints level =
  case level of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 2
    4 -> 3
    _ -> 4

actionLimit :: Level -> Int
actionLimit level =
  case level of
    0 -> 0
    1 -> 2
    2 -> 3
    3 -> 3
    4 -> 4
    5 -> 4
    _ -> 5

movementLimit :: Level -> Int
movementLimit level =
  case level of
    0 -> 0
    1 -> 2
    2 -> 3
    3 -> 4
    _ -> 5

hireLimit :: Level -> Maybe Int
hireLimit level =
  case level of
    0 -> Just 0
    1 -> Just 3
    2 -> Just 5
    3 -> Just 7
    _ -> Nothing




