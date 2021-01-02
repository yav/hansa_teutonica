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

import Basics
import Stats
import Node
import Edge
import Geometry
import Question

data Board = Board
  { boardName              :: Text
    -- ^ Identifier for the map

  , boardNodes             :: Map NodeId Node
    -- ^ State of the nodes on the board

  , boardEdges             :: Map EdgeId Edge
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


modifyEdge :: EdgeId -> (Edge -> Edge) -> Board -> Board
modifyEdge edgeId f board =
  board { boardEdges = Map.adjust f edgeId (boardEdges board) }


--------------------------------------------------------------------------------

-- | The province (if any) that the edge belongs to.
edgeProvince :: Board -> EdgeId -> Maybe ProvinceId
edgeProvince board edgeId = Map.lookup edgeId (boardEdgeProvince board)

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
       city <- Map.lookup capitalId (boardNodes board)
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
  [ ChEdge edgeId spot workerT Nothing
  | (edgeId,edgeState) <- Map.toList (boardEdges board)
  , provinceOk (edgeProvince board edgeId)
  , (spotReq,spot) <- edgeFreeSpots edgeState
  , accepts spotReq workerT
  ]

occupiedSpots ::
  Board ->
  (Maybe ProvinceId -> Bool) ->
  WorkerType ->
  (Worker -> Bool) ->
  [Choice]
occupiedSpots board provinceOk workerT workerOk =
  [ ChEdge edgeId spot workerT (Just worker)
  | (edgeId, edgeState) <- Map.toList (boardEdges board)
  , provinceOk (edgeProvince board edgeId)
  , (spot,spotReq,worker) <- edgeWorkers edgeState
  , accepts spotReq workerT && workerOk worker
  ]


countFull :: Board -> Int
countFull = Map.size . Map.filter nodeIsFull . boardNodes


exportLayout :: Board -> String
exportLayout board = unlines $
  [ "var map = { nodes:" ] ++
  list [ exportNodeSpot nid sid (nodeName n) spot
       | (nid,n) <- Map.toList (boardNodes board)
       , (spot,sid)  <- addFake (nodeFreeSpots n) `zip` [ 0 :: Int .. ]
       ] ++

  [ ", edges:" ] ++
  list [ exportEdgeSpot eid spot sid
       | (eid,ed) <- Map.toList (boardEdges board)
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
    let (from,to) = geoEdgeNodes (boardGeometry board) eid
    in
    "{ edge: " ++ show eid ++
    ", from: " ++ lab from ++ ", to: " ++ lab to ++
    ", spot: " ++ show sid ++ ", req: " ++ req spot ++
    ", prov: " ++ maybe "-1" show (Map.lookup eid (boardEdgeProvince board)) ++
    "}"

  lab i = show (nodeName (boardNodes board Map.! i))
  req x = show $ case x of
                   Require Disc -> "disc" :: Text
                   _ -> "cube"


--------------------------------------------------------------------------------
instance JS.ToJSON Board where
  toJSON b =
    JS.object
      [ "map"     .= boardName b
      , "nodes"   .= doMap (boardNodes b)
      , "edges"   .= doMap (boardEdges b)
      , "fullMax" .= boardMaxFull b
      , "full"    .= countFull b
      , "endVP"   .= doMap (boardEndVP b)
      ]
    where
    doMap m = JS.object [ (fromString (show k) .= v) | (k,v) <- Map.toList m ]


