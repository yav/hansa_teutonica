module Basics where

data Player = Blue | Red | Green | Yellow | Purple
  deriving (Eq,Ord,Show,Enum,Bounded)

data WorkerType = Cube | Circle
  deriving (Eq,Ord,Show,Bounded,Enum)

data Worker = Worker
  { workerOwner :: Player
  , workerType  :: WorkerType
  } deriving (Eq,Ord)





