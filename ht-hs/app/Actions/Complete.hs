module Actions.Complete where

import qualified Data.Map as Map
import Control.Monad(guard,msum,forM_,when)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Maybe(maybeToList)

import Common.Utils
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
          concat [ tryAnnex node playerState
                 , tryOffice nodeId nodeInfo playerId playerState
                                                              edgeId edgeInfo
                 , tryAction node playerState
                 ]

  where
  getEdgeNodes :: EdgeId -> Game -> [(NodeId,Node)]
  getEdgeNodes edgeId s =
    let board = getField gameBoard s
        (x,y) = geoEdgeNodes edgeId (boardGeometry board)
    in [ (n, getField (boardNode n) board) | n <- [x,y] ]

  tryAnnex _ _      = [] -- XXX
  tryAction _ _     = [] -- XXX

  giveVPs edgeId =
    do nodes <- view (getEdgeNodes edgeId)
       forM_ nodes \(_,nodeInfo) ->
         case nodeControlledBy nodeInfo of
           Just playerId ->
             do update (ChangeVP playerId 1)
                update (Log (GainVP playerId 1))
           Nothing -> pure ()

  activateBonus edgeId = pure () -- XXX

  returnWorkers edgeId =
    do workers <- view (edgeWorkers . getField (gameBoard .> boardEdge edgeId))
       forM_ workers \(spotId,_,w) ->
         do update (RemoveWorkerFromEdge edgeId spotId)
            update (ChangeUnavailable w 1)


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


  suitableWorkerFor spot (i,_,w)
    | accepts (spotRequires spot) (workerType w) = Just (i,w)
    | otherwise = Nothing


  resolveAmbig opts =
    Map.elems $
    fmap resolve $
    Map.fromListWith (++) [ (q,[i]) | i@(q,_,_,_) <- opts ]

  resolve opts =
    case opts of
      [(q,h,_,a)] -> (q,h,a)
      _ ->
         let (q,h,_,_) = head opts
         in (q, h, askInputs [ ( playerAnnot q :-> ChEdge e
                               , "Complete this route"
                               , a
                               ) | (_,_,e,a) <- opts ])

