{-# Language NamedFieldPuns #-}
module Geometry
  ( Geometry
  , geoEmpty
  , geoConnect
  , ConsiderEdge(..)
  , geoEdgeNeighbours
  , geoHasPath
  , geoLargestComponent
  ) where

import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.List(nub)
import           Data.Either(partitionEithers)

import Basics

data Geometry = Geometry
  { nodeNeighbours :: Map NodeId [(EdgeId,NodeId)]
  , edgeNeighbours :: Map EdgeId (NodeId,NodeId)
  }

geoEmpty :: Geometry
geoEmpty = Geometry
  { nodeNeighbours = Map.empty
  , edgeNeighbours = Map.empty
  }

geoConnect :: NodeId -> EdgeId -> NodeId -> Geometry -> Geometry
geoConnect from via to Geometry { nodeNeighbours, edgeNeighbours } =
  Geometry { nodeNeighbours = Map.insertWith (++) from [(via,to)]
                            $ Map.insertWith (++) to   [(via,from)]
                              nodeNeighbours
           , edgeNeighbours = Map.insert via (from,to) edgeNeighbours
           }

geoNodeNeighbours :: Geometry -> NodeId -> [(EdgeId,NodeId)]
geoNodeNeighbours Geometry { nodeNeighbours } nodeId =
  Map.findWithDefault [] nodeId nodeNeighbours


data ConsiderEdge = EdgeUsable | EdgeFull | EdgeDisabled
  deriving Eq


geoEdgeNeighbours :: Geometry -> (EdgeId -> ConsiderEdge) -> EdgeId -> [EdgeId]
geoEdgeNeighbours geo consider edgeId =
  case Map.lookup edgeId (edgeNeighbours geo) of
    Just (a,b) -> search (Set.singleton edgeId) [a,b]
    Nothing    -> []
  where
  search visited cities =
    let opts = [ if status == EdgeUsable then Right via else Left (via,to)
               | city <- cities
               , (via,to) <- geoNodeNeighbours geo city
               , let status = consider via
               , not (via `Set.member` visited) && status /= EdgeDisabled
               ]
    in case partitionEithers opts of
         ([],[]) -> []
         (others,[]) ->
            let (newVisited,newCities) = unzip others
            in search (foldr Set.insert visited newVisited) newCities
         (_,es) -> nub es


-- | There a path from one node to another, only using approved nodes
geoHasPath :: Geometry -> (NodeId -> Bool) -> NodeId -> NodeId -> Bool
geoHasPath geo usable from to = search Set.empty [from]
  where
  search visited todo =
    case todo of
      [] -> False
      nodeId : more
        | not (nodeId `Set.member` visited) && usable nodeId ->
          nodeId == to ||
          search (Set.insert nodeId visited)
                 (map snd (geoNodeNeighbours geo nodeId) ++ more)
        | otherwise -> search visited more


geoLargestComponent :: Geometry -> (NodeId -> Bool) -> [NodeId]
geoLargestComponent _geo _belongs = undefined

