module Basics where

type NodeId       = Int      -- ^ Identifies a city
type EdgeId       = Int      -- ^ Identifies a route
type ProvinceId   = Int      -- ^ Identifies a route (Brittania expansion)

data PlayerColor  = Blue | Red | Green | Yellow | Purple
  deriving (Eq,Ord,Show,Enum,Bounded)

data WorkerType   = Cube | Disc
  deriving (Eq,Ord,Show,Bounded,Enum)

data RequireWorker = AnyWorker | Require WorkerType
  deriving (Eq,Ord,Show)

accepts :: RequireWorker -> WorkerType -> Bool
accepts requirement workerType =
  case requirement of
    AnyWorker     -> True
    Require shape -> workerType == shape

data Worker = Worker
  { workerOwner :: PlayerColor
  , workerType  :: WorkerType
  } deriving (Show,Eq,Ord)





