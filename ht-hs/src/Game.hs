module Game where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(nub)

import Basics
import Node
import Edge
import Player
import Question

data Game = Game
  { gamePlayers     :: Map PlayerColor Player
  , gameCities      :: Map NodeId Node
  , gameRoutes      :: Map EdgeId Edge
  , gameGeometry    :: Map NodeId (EdgeId,NodeId)
  , gameTurnOrder   :: [PlayerColor]
  }


freeSpots :: Game -> [Choice]
freeSpots g =
  [ OnEdge nid s Nothing | (nid,e) <- Map.toList (gameRoutes g)
                         , s       <- Set.toList (edgeFreeSpots e) ]

occupiedSpots :: Game -> [ Choice ]
occupiedSpots g =
  [ OnEdge nid s (Just w) | (nid, e) <- Map.toList (gameRoutes g)
                          , (s,w)    <- nub (edgeWorkers e) ]

