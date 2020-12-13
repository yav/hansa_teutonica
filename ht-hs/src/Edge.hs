module Edge
  ( -- * Construction
    Edge
  , edge
  , InitEdge(..)

    -- * Manipulation
  , edgeAddWorker
  , edgeRemoveWorker
  , edgeRemoveWorkers
  , edgeRemoveBonus
  , edgeSetBonus

    -- * Queries
  , edgeWorkers
  , edgeFreeSpots
  , edgeRequires
  , edgeBonusSpot
  , BonusSpot(..)
  ) where

import Data.Maybe(mapMaybe)
import Control.Monad(guard)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.String(fromString)

import qualified Data.Aeson as JS
import Data.Aeson ((.=))

import Basics
import Bonus

data EdgeSpot = EdgeSpot
  { edgeSpotType    :: RequireWorker
  , edgeSpotWorker  :: Maybe Worker
  } deriving Show

setSpotWorker :: Maybe Worker -> EdgeSpot -> EdgeSpot
setSpotWorker mb = \e -> e { edgeSpotWorker = mb }

getSpotWorker :: EdgeSpot -> Maybe (RequireWorker, Worker)
getSpotWorker spot =
  do w <- edgeSpotWorker spot
     pure (edgeSpotType spot, w)

edgeSpot :: RequireWorker -> EdgeSpot
edgeSpot w = EdgeSpot { edgeSpotWorker = Nothing, edgeSpotType = w }

-- | Information about the bonus spot associated with an edge.
data BonusSpot =
    FixedBonus FixedBonus
  | Bonus BonusToken
  | NoBonus
    deriving Show

-- | Information associated with an edge on the map.
data Edge = Edge
  { edgeSpots :: [EdgeSpot]
  , edgeBonus :: BonusSpot
  } deriving Show

-- | Information about the bonus spot on the edge.
edgeBonusSpot :: Edge -> BonusSpot
edgeBonusSpot = edgeBonus

-- | Remove the bonus from this edge.
edgeRemoveBonus :: Edge -> Edge
edgeRemoveBonus ed = case edgeBonus ed of
                       Bonus _ -> ed { edgeBonus = NoBonus }
                       _       -> ed

-- | Place the bonus token in the bonus spot of the edge.
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

-- | Remove a worker of the given type, from one of the spots with
-- the given requirements.
edgeRemoveWorker :: RequireWorker -> Worker -> Edge -> Edge
edgeRemoveWorker t w = modifyEdge match
  where
  match spot = do w1 <- getSpotWorker spot
                  guard ((t,w) == w1)
                  pure (setSpotWorker Nothing spot)

-- | Add a worker to one of the spots with the given requirement.
edgeAddWorker :: RequireWorker -> Worker -> Edge -> Edge
edgeAddWorker t w = modifyEdge match
  where
  match spot = case edgeSpotWorker spot of
                 Nothing | edgeSpotType spot == t ->
                   Just (setSpotWorker (Just w) spot)
                 _ -> Nothing

edgeRequires :: Edge -> [RequireWorker]
edgeRequires = map edgeSpotType . edgeSpots

-- | Compute the various types of free spots on the edge.
edgeFreeSpots :: Edge -> Set RequireWorker
edgeFreeSpots = Set.unions . map getFree . edgeSpots
  where
  getFree spot = case edgeSpotWorker spot of
                   Nothing -> Set.singleton (edgeSpotType spot)
                   Just _  -> Set.empty

-- | Get all workers on the edge.
edgeWorkers :: Edge -> [(RequireWorker, Worker)]
edgeWorkers = mapMaybe getSpotWorker . edgeSpots

-- | Remove all workers from an edge.
edgeRemoveWorkers :: Edge -> Edge
edgeRemoveWorkers ed =
                 ed { edgeSpots = map (setSpotWorker Nothing) (edgeSpots ed) }


-- | Desription of an empty edge
data InitEdge =
  InitEdge
    { initEdgeBonus :: Maybe FixedBonus
    , initEdgeSpots :: [RequireWorker]
    }

-- | A blank edge with the given requirements.
edge :: InitEdge -> Edge
edge InitEdge { initEdgeBonus, initEdgeSpots } =
  Edge
    { edgeSpots = map edgeSpot initEdgeSpots
    , edgeBonus = case initEdgeBonus of
                    Nothing    -> NoBonus
                    Just bonus -> FixedBonus bonus
    }


--------------------------------------------------------------------------------


instance JS.ToJSON Edge where
  toJSON e = JS.object (optionalBonus [ "workers" .= JS.object workerFields ])
     where
     optionalBonus others =
       case edgeBonus e of
         Bonus b -> ("bonus" .= b) : others
         _       -> others

     keys = [ 0 :: Int .. ]

     workerFields =
       [ fromString (show i) .= w
       | (i, EdgeSpot { edgeSpotWorker = Just w }) <- keys `zip` edgeSpots e
       ]


