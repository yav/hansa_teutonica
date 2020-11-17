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
  , ActionSpace(..)
  , nodeGetActions
  , nodeAddActionWorker
  ) where

import Data.Maybe(listToMaybe)
import Data.List(maximumBy)
import Data.Function(on)
import qualified Data.Map as Map

import Basics

data Node = Node
  { emptySpots    :: [(WorkerType,Int)] -- ^ Left-most first
  , fullSpots     :: [Worker]
  , nodeExtra     :: [Worker]
  , nodeActions   :: [ActionSpace]
  }

node :: [Maybe RequireWorker] -> [(WorkerType,Int)] -> Node
node acts ws =
  Node { fullSpots = []
       , emptySpots = ws
       , nodeExtra = []
       , nodeActions = map nodeActionSpace acts
       }

data ActionSpace =
    EmptyWorkerSpace RequireWorker
  | FullWorkerSpace Worker
  | NoWorkerSpace

nodeActionSpace :: Maybe RequireWorker -> ActionSpace
nodeActionSpace mb =
  case mb of
    Nothing -> NoWorkerSpace
    Just r  -> EmptyWorkerSpace r

nodeGetActions :: Node -> [ActionSpace]
nodeGetActions = nodeActions


nodeAddActionWorker :: Int -> Worker -> Node -> Node
nodeAddActionWorker i w n =
  n { nodeActions = case splitAt i (nodeActions n) of
                      (as, _ : bs) -> (as ++ FullWorkerSpace w : bs)
                      _ -> nodeActions n }


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

nodeRightMost :: Node -> Maybe PlayerColor
nodeRightMost = fmap workerOwner . listToMaybe . fullSpots

nodeControlledBy :: Node -> Maybe PlayerColor
nodeControlledBy n =
  case vals of
    [] -> Nothing
    _  -> Just $ fst $ maximumBy (compare `on` snd) vals
  where
  addVal (w,we) = Map.insertWith (+) (workerOwner w) (1+we :: Double)
  vals  = Map.toList
        $ foldr addVal Map.empty
        $ zip (fullSpots n ++ reverse (nodeExtra n))
        $ iterate (/2) (1/2)


