module Edge
  ( Edge
  , edge
  , edgeAddWorker
  , edgeRemoveWorker
  , edgeReset
  , edgeWorkers
  , edgeFreeSpots
  ) where

import qualified Data.List as List
import Data.Maybe(isJust,mapMaybe)
import Control.Monad(guard)
import Data.Set(Set)
import qualified Data.Set as Set

import Basics

data EdgeSpot = EdgeSpot
  { edgeSpotType    :: WorkerType
  , edgeSpotWorker  :: Maybe Player
  }

setSpotWorker :: Maybe Player -> EdgeSpot -> EdgeSpot
setSpotWorker mb = \e -> e { edgeSpotWorker = mb }

getSpotWorker :: EdgeSpot -> Maybe Worker
getSpotWorker spot =
  do p <- edgeSpotWorker spot
     pure Worker { workerType = edgeSpotType spot, workerOwner = p }

edgeSpot :: WorkerType -> EdgeSpot
edgeSpot w = EdgeSpot { edgeSpotWorker = Nothing, edgeSpotType = w }

newtype Edge = Edge [EdgeSpot]

modifyEdge :: (EdgeSpot -> Maybe EdgeSpot) -> Edge -> Edge
modifyEdge f (Edge es0) = Edge (search es0)
  where
  search es =
    case es of
      [] -> []
      e : more ->
        case f e of
          Just e1 -> e1 : more
          Nothing -> e : search es

edgeRemoveWorker :: Worker -> Edge -> Edge
edgeRemoveWorker w = modifyEdge match
  where
  match spot = do w1 <- getSpotWorker spot
                  guard (w == w1)
                  pure (setSpotWorker Nothing spot)

edgeAddWorker :: Worker -> Edge -> Edge
edgeAddWorker w = modifyEdge match
  where
  match spot = case edgeSpotWorker spot of
                 Nothing | edgeSpotType spot == workerType w ->
                   Just (setSpotWorker (Just (workerOwner w)) spot)
                 _ -> Nothing

edgeFreeSpots :: Edge -> Set WorkerType
edgeFreeSpots (Edge es) = Set.unions (map getFree es)
  where
  getFree spot = case edgeSpotWorker spot of
                   Nothing -> Set.singleton (edgeSpotType spot)

edgeWorkers :: Edge -> [Worker]
edgeWorkers (Edge es) = mapMaybe getSpotWorker es

edgeReset :: Edge -> Edge
edgeReset (Edge es) = Edge (map (setSpotWorker Nothing) es)

edge :: [WorkerType] -> Edge
edge = Edge . map edgeSpot
