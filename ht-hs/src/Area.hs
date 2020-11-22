module Area where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(nub)
import Data.Text(Text)
import Control.Applicative((<|>))
import Control.Monad(guard)

import Basics
import Node
import Edge
import Geometry
import Question

data Area = Area
  { areaNodes             :: Map NodeId Node
  , areaEdges             :: Map EdgeId Edge
  , areaGeometry          :: Geometry
  , areaEdgeProvince      :: Map EdgeId ProvinceId
  , areaProvinceCapitals  :: Map ProvinceId NodeId
  , areaProvinceNodes     :: Map ProvinceId (Set NodeId)
  , areaCapital           :: Maybe ProvinceId
  }

data InitCapital = ProvinceCapital | AreaCapital

data InitArea = InitArea
  { initName      :: Text
  , initNode      :: InitNode
  , initProvinces :: [Text]
  , initCapital   :: Maybe InitCapital
  , initEdges     :: [InitAreaEdge]
  }

data InitAreaEdge = InitAreaEdge
  { initTarget    :: Text
  , initProvince  :: Text
  , initEdge      :: InitEdge
  }

emptyArea :: Area
emptyArea = Area
  { areaNodes = Map.empty
  , areaEdges = Map.empty
  , areaGeometry = geoEmpty
  , areaEdgeProvince = Map.empty
  , areaProvinceCapitals = Map.empty
  , areaProvinceNodes = Map.empty
  , areaCapital = Nothing
  }


--------------------------------------------------------------------------------

data ProvinceAccess = UnrestrictedAccess | AccessVia NodeId
  deriving (Show)


edgeProvince :: Area -> EdgeId -> Maybe ProvinceId
edgeProvince area edgeId = Map.lookup edgeId (areaEdgeProvince area)


provinceAccessible ::
  PlayerColor          {- ^ By this player -} ->
  Set NodeId           {- ^ Used up gateways -} ->
  Area                 {- ^ Information about the current state -} ->
  ProvinceId           {- ^ Province in question -} ->
  Maybe ProvinceAccess {- ^ Is it accessible, and if so how -}
provinceAccessible player used area provinceId =
  case Map.lookup provinceId (areaProvinceCapitals area) of
    Nothing      -> pure UnrestrictedAccess
    Just capital -> tryWith (Just capital) <|> tryWith (areaCapital area)
      where
      tryWith candidate =
        do capitalId <- candidate
           guard (not (capitalId `Set.member` used))
           city <- Map.lookup capitalId (areaNodes area)
           rightPlayer <- nodeRightMost city
           guard (player == rightPlayer)
           pure (AccessVia capitalId)


freeSpots :: Area -> [Choice]
freeSpots g =
  [ ChEdge nid s Nothing | (nid,e) <- Map.toList (areaEdges g)
                         , s       <- Set.toList (edgeFreeSpots e) ]

occupiedSpots :: Area -> [Choice]
occupiedSpots g =
  [ ChEdge nid s (Just w) | (nid, e) <- Map.toList (areaEdges g)
                          , (s,w)    <- nub (edgeWorkers e) ]

