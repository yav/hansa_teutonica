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

import           Data.IntMap(IntMap)
import qualified Data.IntMap as Map
import qualified Data.Set as Set
import           Data.List(nub,mapAccumL,maximumBy)
import           Data.Either(partitionEithers)

import Basics

data Geometry = Geometry
  { nodeNeighbours :: IntMap {-NodeId-} [(EdgeId,NodeId)]
  , edgeNeighbours :: IntMap {-EdgeId-} (NodeId,NodeId)
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


type Reps = IntMap NodeId

geoLargestComponent :: Geometry -> (NodeId -> Bool) -> Int
geoLargestComponent Geometry { edgeNeighbours, nodeNeighbours } belongs =
     largest
   $ Map.toList
   $ Map.fromListWith (+)
   $ snd
   $ mapAccumL count classes
   $ Map.keys nodeNeighbours

  where
  find :: NodeId -> Reps -> (NodeId,Reps)
  find r reps =
    case Map.lookup r reps of
      Just r1 -> let (r2,reps1) = find r1 reps
                 in (r2,Map.insert r r2 reps1)
      Nothing -> (r,reps)

  union :: NodeId -> NodeId -> Reps -> Reps
  union x y reps =
    let (rx,reps1) = find x reps
        (ry,reps2) = find y reps1
    in if rx == ry then reps2 else Map.insert rx ry reps2

  maybeUnion :: (NodeId,NodeId) -> Reps -> Reps
  maybeUnion (n1,n2) reps =
    if belongs n1 && belongs n2
      then union n1 n2 reps
      else reps

  classes :: Reps
  classes = foldr maybeUnion Map.empty (Map.elems edgeNeighbours)

  count :: Reps -> NodeId -> (Reps,(NodeId,Int))
  count reps n = (reps1, (r, if belongs n then 1 else 0))
    where (r,reps1) = find n reps

  cmp (_,x) (_,y) = compare x y

  largest xs = if null xs then 0 else snd (maximumBy cmp xs)


