module Node
  ( -- * Construction
    Node
  , node
  , NodeAction(..)

    -- * Manipulation
  , nodeAddWorker
  , nodeAddExtra
  , nodeSwap

    -- * Queries
  , nodeActions
  , nodeWorkers
  , nodeNextFree
  , nodeRightMost
  , nodeControlledBy
  ) where

import Data.Maybe(listToMaybe)
import Data.List(maximumBy)
import Data.Function(on)
import qualified Data.Map as Map

import Basics
import Stats

-- | Special actions associated with a node
data NodeAction = UpdgradeStat Stat | GainEndGamePoints

-- | A node on the map
data Node = Node
  { emptySpots    :: [(WorkerType,Int)] -- ^ Left-most first
  , fullSpots     :: [Worker]
  , nodeExtra     :: [Worker]
  , nodeActions'  :: [NodeAction]
  }

-- | Build an empty node with the given action and office spaces.
node :: [NodeAction] -> [(WorkerType,Int)] -> Node
node acts ws =
  Node { fullSpots = []
       , emptySpots = ws
       , nodeExtra = []
       , nodeActions' = acts
       }

-- | Get the special actions associated with this node
nodeActions :: Node -> [NodeAction]
nodeActions = nodeActions'

-- | Add a worker to the next avaiable "overflow" space.
-- Overflow spaces grow to the left, so this worker becomes the left-most
-- worker.
nodeAddExtra :: Worker -> Node -> Node
nodeAddExtra w = \n -> n { nodeExtra = w : nodeExtra n }

-- | Add a worker to the next avaialbe office space.
-- This worker becomes the right-most worker.
nodeAddWorker :: Worker -> Node -> Node
nodeAddWorker w = \n -> n { fullSpots = w : fullSpots n
                          , emptySpots = drop 1 (emptySpots n)
                          }

-- | Swap two workers in this node.
-- The number is the index of a worker (counting from the right),
-- and it is swapped with the worker beofre (i.e., one spot to the left)
nodeSwap :: Int -> Node -> Node
nodeSwap i = \n -> n { fullSpots = case splitAt i (fullSpots n) of
                                     (as,b:c:ds) -> as ++ c : b : ds
                                     _ -> fullSpots n }

-- | Get the next free worker space in this node.
nodeNextFree :: Node -> Maybe (WorkerType,Int)
nodeNextFree = listToMaybe . emptySpots

-- | Get a list of the workers in this node, rightmost first.
nodeWorkers :: Node -> [Worker]
nodeWorkers = fullSpots

-- | Get the player who has the right-most worker in a node, if any.
nodeRightMost :: Node -> Maybe PlayerColor
nodeRightMost = fmap workerOwner . listToMaybe . fullSpots

-- | Compute who is currently in control if the node, if any.
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


