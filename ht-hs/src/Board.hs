{-# Language TemplateHaskell #-}
module Board where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.String(fromString)
import Control.Applicative((<|>))
import Control.Monad(guard)

import qualified Data.Aeson as JS
import Data.Aeson ((.=))

import Common.Field

import Basics
import Stats
import Node
import Edge
import Geometry
import Question

data Board = Board
  { boardName              :: Text
    -- ^ Identifier for the map

  , _boardNodes            :: Map NodeId Node
    -- ^ State of the nodes on the board

  , _boardEdges            :: Map EdgeId Edge
    -- ^ State of the edges on the board

  , boardGeometry          :: Geometry
    -- ^ Connections on the board

  , boardEdgeProvince      :: Map EdgeId ProvinceId
    -- ^ Special edge restrictions.

  , boardProvinces         :: Map ProvinceId Province
    -- ^ Informations about the special regions.

  , boardCapital           :: Maybe ProvinceId
    -- ^ This node gives access to all provinces

  , boardMaxFull           :: Int
    -- ^ This many full cities to end the game

  , boardInitialTokens     :: Set EdgeId
    -- ^ Where to place initial tokens

  , boardEndVP             :: Map Level PlayerId
    -- ^ Occupied spots on end-game point track, indexed by privilege
  } deriving Show


data Province = Province
  { provinceName    :: Text           -- ^ Name of the province
  , provinceCapital :: NodeId         -- ^ This city gives access to province
  , provinceNodes   :: Set NodeId     -- ^ These cities are in the province
  } deriving Show



declareFields ''Board

boardNode :: NodeId -> Field Board Node
boardNode nodeId = boardNodes .> mapAt nodeId

boardEdge :: EdgeId -> Field Board Edge
boardEdge edgeId = boardEdges .> mapAt edgeId




--------------------------------------------------------------------------------

-- | The province (if any) that the edge belongs to.
edgeProvince :: EdgeId -> Board -> Maybe ProvinceId
edgeProvince edgeId board = Map.lookup edgeId (boardEdgeProvince board)

-- | Is the province with the given captial accessible, and if so how.
provinceAccessible ::
  PlayerId             {- ^ By this player -} ->
  Set NodeId           {- ^ Used up gateways -} ->
  Board                {- ^ Information about the current state -} ->
  Province             {- ^ Info about the province -} ->
  Maybe NodeId         {- ^ Is it accessible, and if so using which gatweay. -}
provinceAccessible player used board prov =
  tryWith (Just (provinceCapital prov)) <|> tryWith (boardCapital board)
  where
  tryWith candidate =
    do capitalId <- candidate
       guard (not (capitalId `Set.member` used))
       let city = getField (boardNode capitalId) board
       rightPlayer <- nodeRightMost city
       guard (player == rightPlayer)
       pure capitalId

-- | Provinces avialbe for (re)palcement.
accessibleProvinces ::
  PlayerId        {- ^ By this player -} ->
  Set NodeId      {- ^ Used up gatweays -} ->
  Board           {- ^ State of the board -} ->
  Map ProvinceId NodeId
accessibleProvinces player used board =
  Map.mapMaybe (provinceAccessible player used board)
               (boardProvinces board)


-- | Find free spots satisfying the given restrictions.
freeSpots ::
  Board                      {- ^ State of the board -} ->
  (Maybe ProvinceId -> Bool) {- ^ Province restrictions -} ->
  WorkerType                 {- ^ Spot for this kind of worker -} ->
  [Choice]                   {- ^ Available spots -}
freeSpots board provinceOk workerT =
  [ ChEdgeEmpty edgeId spot workerT
  | (edgeId,edgeState) <- Map.toList (getField boardEdges board)
  , provinceOk (edgeProvince edgeId board)
  , (spotReq,spot) <- edgeFreeSpots edgeState
  , accepts spotReq workerT
  ]

replaceSpots ::
  Board ->
  (Maybe ProvinceId -> Bool) ->
  WorkerType ->
  (Worker -> Bool) ->
  [Choice]
replaceSpots board provinceOk workerT workerOk =
  [ ChEdgeFull edgeId spot (Just workerT) worker
  | (edgeId, edgeState) <- Map.toList (getField boardEdges board)
  , provinceOk (edgeProvince edgeId board)
  , (spot,spotReq,worker) <- edgeWorkers edgeState
  , accepts spotReq workerT && workerOk worker
  ]

replaceTargets ::
  Board ->
  (Maybe ProvinceId -> Bool) ->
  EdgeId ->
  WorkerType ->
  [Choice]
replaceTargets board provinceOk edgeId workerT =
  [ ChEdgeEmpty e spot workerT
  | (e,spots) <- geoEdgeNeighbours (boardGeometry board) edgeStatus edgeId
  , spot <- spots
  ]
  where
  edgeStatus candidate
    | candidate == edgeId = EdgeDisabled
    | provinceOk (edgeProvince candidate board) =
      let info  = getField (boardEdge candidate) board
          spots = [ spot | (req,spot) <- edgeFreeSpots info
                         , accepts req workerT ]
      in case spots of
           [] -> EdgeFull
           _  -> EdgeUsable spots
    | otherwise = EdgeDisabled


pickupSpots ::
  Board ->
  (Maybe ProvinceId -> Bool) ->
  (Worker -> Bool) ->
  [Choice]
pickupSpots board provinceOk workerOk =
  [ ChEdgeFull edgeId spot Nothing worker
  | (edgeId, edgeState) <- Map.toList (getField boardEdges board)
  , provinceOk (edgeProvince edgeId board)
  , (spot,_,worker) <- edgeWorkers edgeState
  , workerOk worker
  ]

countFull :: Board -> Int
countFull = Map.size . Map.filter nodeIsFull . getField boardNodes

fullEdgesFor :: PlayerId -> Board -> [(EdgeId,Edge)]
fullEdgesFor playerId =
  Map.toList . Map.filter (edgeReadyFor playerId) . getField boardEdges

--------------------------------------------------------------------------------
instance JS.ToJSON Board where
  toJSON b =
    JS.object
      [ "map"     .= boardName b
      , "nodes"   .= doMap (getField boardNodes b)
      , "edges"   .= doMap (getField boardEdges b)
      , "fullMax" .= boardMaxFull b
      , "full"    .= countFull b
      , "endVP"   .= doMap (boardEndVP b)
      ]
    where
    doMap m = JS.object [ (fromString (show k) .= v) | (k,v) <- Map.toList m ]


-- for editor.  XXX: redo using aeson
exportLayout :: Board -> String
exportLayout board = unlines $
  [ "var map = { nodes:" ] ++
  list [ exportNodeSpot nid sid (nodeName n) spot
       | (nid,n) <- Map.toList (getField boardNodes board)
       , (spot,sid)  <- addFake (nodeFreeSpots n) `zip` [ 0 :: Int .. ]
       ] ++

  [ ", edges:" ] ++
  list [ exportEdgeSpot eid spot sid
       | (eid,ed) <- Map.toList (getField boardEdges board)
       , (spot,sid) <- edgeRequires ed `zip` [ 0 :: Int .. ]
       ] ++
  [ "}" ]

  where
  -- we do so that we know the locations of empty cities
  addFake xs = case xs of
                 [] -> [ NodeSpot { spotPrivilege = 1, spotVP = 0
                                  , spotRequires = Require Cube } ]
                 _  -> xs

  -- assumes non-empty
  list xs = [ sep ++ x | sep <- "[" : repeat ","
                       | x   <- xs ] ++
            ["]"]

  exportNodeSpot nid sid name spot =
    "{ node: " ++ show nid ++ ", name: " ++ show name ++
    ", id: " ++ show sid ++
    ", vp: " ++ show (spotVP spot) ++ ", priv: " ++ show (spotPrivilege spot) ++
    ", req: " ++ req (spotRequires spot) ++
    "}"

  exportEdgeSpot eid spot sid =
    let (from,to) = geoEdgeNodes eid (boardGeometry board)
    in
    "{ edge: " ++ show eid ++
    ", from: " ++ lab from ++ ", to: " ++ lab to ++
    ", spot: " ++ show sid ++ ", req: " ++ req spot ++
    ", prov: " ++ maybe "-1" show (Map.lookup eid (boardEdgeProvince board)) ++
    "}"

  lab i = show (nodeName (getField (boardNode i) board))
  req x = show $ case x of
                   Require Disc -> "disc" :: Text
                   _ -> "cube"



