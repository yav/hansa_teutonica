module Edge
  ( Edge
  , edge
  , EdgeSpotType(..)
  , edgeAddWorker
  , edgeRemoveWorker
  , edgeReset
  , edgeWorkers
  , edgeFreeSpots
  , BonusSpot
  , edgeBonus
  , edgeRemoveBonus
  , edgeSetBonus
  ) where

import qualified Data.List as List
import Data.Maybe(isJust,mapMaybe)
import Control.Monad(guard)
import Data.Set(Set)
import qualified Data.Set as Set

import Basics
import Bonus

data EdgeSpotType = AnyWorker | Require WorkerType
  deriving (Eq,Ord,Show)


data EdgeSpot = EdgeSpot
  { edgeSpotType    :: EdgeSpotType
  , edgeSpotWorker  :: Maybe Worker
  } deriving Show

setSpotWorker :: Maybe Worker -> EdgeSpot -> EdgeSpot
setSpotWorker mb = \e -> e { edgeSpotWorker = mb }

getSpotWorker :: EdgeSpot -> Maybe (EdgeSpotType, Worker)
getSpotWorker spot =
  do w <- edgeSpotWorker spot
     pure (edgeSpotType spot, w)

edgeSpot :: EdgeSpotType -> EdgeSpot
edgeSpot w = EdgeSpot { edgeSpotWorker = Nothing, edgeSpotType = w }

data BonusSpot =
    FixedBonus FixedBonus
  | Bonus BonusToken
  | NoBonus
    deriving Show

data Edge = Edge
  { edgeSpots :: [EdgeSpot]
  , edgeBonus :: BonusSpot
  } deriving Show

edgeBonusSpot :: Edge -> BonusSpot
edgeBonusSpot = edgeBonus

edgeRemoveBonus :: Edge -> Edge
edgeRemoveBonus ed = case edgeBonus ed of
                       Bonus _ -> ed { edgeBonus = NoBonus }
                       _       -> ed

edgeSetBonus :: BonusToken -> Edge -> Edge
edgeSetBonus b ed = case edgeBonus ed of
                      NoBonus -> ed { edgeBonus = Bonus b }
                      _       -> ed


modifyEdge :: (EdgeSpot -> Maybe EdgeSpot) -> Edge -> Edge
modifyEdge f ed = ed { edgeSpots = search (edgeSpots ed) }
  where
  search es =
    case es of
      [] -> []
      e : more ->
        case f e of
          Just e1 -> e1 : more
          Nothing -> e : search es

edgeRemoveWorker :: EdgeSpotType -> Worker -> Edge -> Edge
edgeRemoveWorker t w = modifyEdge match
  where
  match spot = do w1 <- getSpotWorker spot
                  guard ((t,w) == w1)
                  pure (setSpotWorker Nothing spot)

edgeAddWorker :: EdgeSpotType -> Worker -> Edge -> Edge
edgeAddWorker t w = modifyEdge match
  where
  match spot = case edgeSpotWorker spot of
                 Nothing | edgeSpotType spot == t ->
                   Just (setSpotWorker (Just w) spot)
                 _ -> Nothing

edgeFreeSpots :: Edge -> Set EdgeSpotType
edgeFreeSpots = Set.unions . map getFree . edgeSpots
  where
  getFree spot = case edgeSpotWorker spot of
                   Nothing -> Set.singleton (edgeSpotType spot)
                   Just _  -> Set.empty

edgeWorkers :: Edge -> [(EdgeSpotType, Worker)]
edgeWorkers = mapMaybe getSpotWorker . edgeSpots

edgeReset :: Edge -> Edge
edgeReset ed = ed { edgeSpots = map (setSpotWorker Nothing) (edgeSpots ed) }

edge :: Maybe FixedBonus -> [EdgeSpotType] -> Edge
edge mb spots = Edge
  { edgeSpots = map edgeSpot spots
  , edgeBonus = case mb of
                  Nothing -> NoBonus
                  Just p  -> FixedBonus p
  }
