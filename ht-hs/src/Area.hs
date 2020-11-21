{-# Language BlockArguments, RecordWildCards #-}
module Area where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(nub)
import Control.Applicative((<|>))
import Control.Monad(guard)

import Basics
import Node
import Edge
import Geometry
import Question

data Area = Area
  { areaCities    :: Map NodeId Node
  , areaRoutes    :: Map EdgeId Edge
  , areaGeometry  :: Geometry
  , areaProvinces :: Map ProvinceId Province
  , areaCapital   :: Maybe ProvinceId
  }

data Province = Province
  { provinceCities    :: Set NodeId   -- ^ Cities may be in more than 1 province
  , provinceRoutes    :: Set EdgeId
  , provinceCapital   :: Maybe NodeId
  }


data ProvinceAccess = UnrestrictedAccess | AccessVia NodeId
  deriving (Show)

provinceAccessible ::
  PlayerColor          {- ^ By this player -} ->
  Set NodeId           {- ^ Used up gateways -} ->
  Area                 {- ^ Information about the current state -} ->
  ProvinceId           {- ^ Province in question -} ->
  Maybe ProvinceAccess {- ^ Is it accessible, and if so how -}
provinceAccessible player used area provinceId =
  case Map.lookup provinceId (areaProvinces area) of
    Nothing -> pure UnrestrictedAccess
    Just province ->
      tryWith (provinceCapital province) <|> tryWith (areaCapital area)
      where
      tryWith candidate =
        do capitalId <- candidate
           guard (not (capitalId `Set.member` used))
           city <- Map.lookup capitalId (areaCities area)
           rightPlayer <- nodeRightMost city
           guard (player == rightPlayer)
           pure (AccessVia capitalId)


freeSpots :: Area -> [Choice]
freeSpots g =
  [ ChEdge nid s Nothing | (nid,e) <- Map.toList (areaRoutes g)
                         , s       <- Set.toList (edgeFreeSpots e) ]

occupiedSpots :: Area -> [Choice]
occupiedSpots g =
  [ ChEdge nid s (Just w) | (nid, e) <- Map.toList (areaRoutes g)
                          , (s,w)    <- nub (edgeWorkers e) ]

