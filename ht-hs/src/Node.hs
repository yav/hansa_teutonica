module Node
  ( Node
  , node
  , nodeAddWorker
  , nodeAddExtra
  , nodeSwap
  , nodeNextFree
  , nodeWorkers
  , nodeRightMost
  , nodeControlledBy
  ) where

import Data.Maybe(listToMaybe)
import Data.List(maximumBy)
import Data.Function(on)
import Data.Map(Map)
import qualified Data.Map as Map

import Basics

data Node = Node
  { emptySpots  :: [(WorkerType,Int)]
  , fullSpots   :: [Worker]
  , nodeExtra   :: [Worker]
  }

node :: [(WorkerType,Int)] -> Node
node ws = Node { fullSpots = [], emptySpots = ws, nodeExtra = [] }

nodeAddExtra :: Worker -> Node -> Node
nodeAddExtra w = \n -> n { nodeExtra = w : nodeExtra n }

nodeAddWorker :: Worker -> Node -> Node
nodeAddWorker w = \n -> n { fullSpots = w : fullSpots n
                          , emptySpots = drop 1 (emptySpots n)
                          }

-- | The int is the index of a worker from the right.
-- Swap with the worker before it.
nodeSwap :: Int -> Node -> Node
nodeSwap i = \n -> n { fullSpots = case splitAt i (fullSpots n) of
                                     (as,b:c:ds) -> as ++ c : b : ds
                                     _ -> fullSpots n }

nodeNextFree :: Node -> Maybe (WorkerType,Int)
nodeNextFree = listToMaybe . emptySpots

-- | Rightmost first.
nodeWorkers :: Node -> [Worker]
nodeWorkers = fullSpots

nodeRightMost :: Node -> Maybe Player
nodeRightMost = fmap workerOwner . listToMaybe . fullSpots

nodeControlledBy :: Node -> Maybe Player
nodeControlledBy n =
  case vals of
    [] -> Nothing
    _  -> Just $ fst $ maximumBy (compare `on` snd) vals
  where
  addVal (w,we) = Map.insertWith (+) (workerOwner w) (1+we)
  vals  = Map.toList
        $ foldr addVal Map.empty
        $ zip (fullSpots n ++ reverse (nodeExtra n))
        $ iterate (/2) (1/2)


