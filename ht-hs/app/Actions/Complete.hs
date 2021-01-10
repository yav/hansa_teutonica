module Actions.Complete (tryCompleteEdge) where

import qualified Data.Map as Map
import Control.Monad(guard,msum,forM_,when)
import Data.Text(Text)
import Data.Maybe(maybeToList)

import Common.Interact
import Common.Field

import Basics
import Stats
import Player
import Board
import Geometry
import Edge
import Node
import Question
import Game
import Turn
import Event

import Actions.Common


tryCompleteEdge :: PlayerOptions
tryCompleteEdge state =
  resolveAmbig
  do (turn,playerState) <- startAction state
     let playerId = currentPlayer turn
         board    = getField gameBoard state
     (edgeId,edgeInfo) <- fullEdgesFor playerId board
     tryJustComplete edgeId playerId ++
       do (nodeId,nodeInfo) <- getEdgeNodes edgeId state
          concat [ tryAnnex nodeInfo playerState
                 , tryOffice nodeId nodeInfo playerId playerState
                                                              edgeId edgeInfo
                 , tryAction nodeInfo playerState
                 ]

type ChoiceWithEdge = (WithPlayer Choice, Text, EdgeId, Interact ())

resolveAmbig :: [ChoiceWithEdge] -> [PlayerChoice]
resolveAmbig = Map.elems . fmap resolve . Map.fromListWith (++) . map toMap
  where
  toMap i@(q,_,_,_) = (q,[i])

  resolve opts =
    case opts of
      [(q,h,_,a)] -> (q,h,a)
      _ ->
         let (q,h,_,_) = head opts
         in (q, h, askInputs [ ( playerAnnot q :-> ChEdge e
                               , "Complete this route"
                               , a
                               ) | (_,_,e,a) <- opts ])



getEdgeNodes :: EdgeId -> Game -> [(NodeId,Node)]
getEdgeNodes edgeId s =
  let board = getField gameBoard s
      (x,y) = geoEdgeNodes edgeId (boardGeometry board)
  in [ (n, getField (boardNode n) board) | n <- [x,y] ]



--------------------------------------------------------------------------------

giveVPs :: EdgeId -> Interact ()
giveVPs edgeId =
  do nodes <- view (getEdgeNodes edgeId)
     forM_ nodes \(_,nodeInfo) ->
       case nodeControlledBy nodeInfo of
         Just playerId ->
           do update (ChangeVP playerId 1)
              update (Log (GainVP playerId 1))
         Nothing -> pure ()

activateBonus :: EdgeId -> Interact ()
activateBonus edgeId = pure () -- XXX

returnWorkers :: EdgeId -> Interact ()
returnWorkers edgeId =
  do workers <- view (edgeWorkers . getField (gameBoard .> boardEdge edgeId))
     forM_ workers \(spotId,_,w) ->
       do update (RemoveWorkerFromEdge edgeId spotId)
          update (ChangeUnavailable w 1)

--------------------------------------------------------------------------------

-- | Complete an edge without extra benefits
tryJustComplete :: EdgeId -> PlayerId -> [ChoiceWithEdge]
tryJustComplete edgeId playerId =
  [ ( playerId :-> ChEdge edgeId
    , "Complete with NO office/action"
    , edgeId
    , doAction
      do update (Log (CompleteRoute edgeId))
         giveVPs edgeId
         activateBonus edgeId
         returnWorkers edgeId
    )
  ]

-- | Complete an edge and build and office
tryOffice ::
  NodeId -> Node ->
  PlayerId -> Player ->
  EdgeId -> Edge ->
  [ ChoiceWithEdge ]
tryOffice nodeId nodeInfo playerId playerState edgeId edgeInfo =
  do spot <- maybeToList (nodeNextFree nodeInfo)
     guard (spotPrivilege spot <= getLevel Privilege playerState)
     (edgeSpotId,worker) <- maybeToList
              $ msum $ map (suitableWorkerFor spot) $ edgeWorkers edgeInfo
     pure ( workerOwner worker :-> ChNodeEmpty nodeId (workerType worker)
          , "Build office"
          , edgeId
          , doAction
            do update (Log (CompleteRoute edgeId))
               giveVPs edgeId
               update (RemoveWorkerFromEdge edgeId edgeSpotId)
               update (PlaceWorkerInOffice nodeId worker)
               update (Log (BuildOffice nodeId worker))
               when (spotVP spot > 0)
                  do update (ChangeVP playerId (spotVP spot))
                     update (Log (GainVP playerId (spotVP spot)))
               activateBonus edgeId
               returnWorkers edgeId
          )
  where
  suitableWorkerFor spot (i,_,w)
    | accepts (spotRequires spot) (workerType w) = Just (i,w)
    | otherwise = Nothing


-- | Complete an edge and build an annex using a bonus token
tryAnnex :: Node -> Player -> [ChoiceWithEdge]
tryAnnex _ _      = [] -- XXX

-- | Complete an edge and use a special action
tryAction :: Node -> Player -> [ChoiceWithEdge]
tryAction _ _     = [] -- XXX


