module Actions.Place where

import qualified Data.Map as Map
import Control.Monad(guard)

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Player
import Board
import Question
import Game
import Turn
import Event

import Actions.Common

tryPlace :: PlayerOptions
tryPlace state =
  do (turn,playerState) <- startAction state
     let workerT      = getWorkerPreference playerState

         board        = getField gameBoard state
         player       = currentPlayer turn

         gateways     = accessibleProvinces player (usedGateways turn) board
         accessible   = maybe True (`Map.member` gateways)
         gatewayFor edgeId =
                        (`Map.lookup` gateways) =<< edgeProvince edgeId board

         totWorkers   = sum (map (`getAvailable` playerState) enumAll)
         canReplace w = workerOwner w /= player &&
                        totWorkers > replacementCost (workerType w)

         getFree      = do guard (getAvailable workerT playerState > 0)
                           x <- freeSpots board accessible workerT
                           pure (x, "Place a worker")
         getFull      = do x <- replaceSpots board accessible workerT canReplace
                           pure (x, "Replace a worker")
         changePref   = do let otherT = otherType workerT
                           guard (getAvailable otherT playerState > 0)
                           let help = "Change preference to " <>
                                      jsKey otherT
                           pure (ChSetPreference otherT, help)

     (ch,help) <- [ (player :-> c,help) |
                     (c,help) <- changePref ++ getFree ++ getFull ]
     pure (ch, help, handleChoice gatewayFor ch)

  where
  handleChoice gatewayFor (pid :-> ch) =
    case ch of
      ChSetPreference t ->
        do let w = Worker { workerOwner = pid, workerType = t }
           update (SetWorkerPreference w)

      ChEdgeEmpty edgeId spot workerT ->
        doAction
        do let w = Worker { workerOwner = pid, workerType = workerT }
           update (ChangeAvailble w (-1))
           case gatewayFor edgeId of
             Just g -> update (UseGateway g)
             _      -> pure ()
           update (PlaceWorkerOnEdge edgeId spot w)
           update (Log (PlaceWorker w edgeId spot))

      ChEdgeFull edgeId spot ~(Just workerT) worker ->
        doAction
        do board <- view (getField gameBoard)
           update (RemoveWorkerFromEdge edgeId spot)
           update (AddWorkerToHand (edgeProvince edgeId board) worker)
           let ourWorker = Worker { workerOwner = pid, workerType = workerT }
           update (ChangeAvailble ourWorker (-1))
           update (PlaceWorkerOnEdge edgeId spot ourWorker)
           replaceFee pid 1 (replacementCost (workerType worker))
           update (Log (ReplaceWorker worker ourWorker edgeId spot))
           otherPlayerMoveAndPlace edgeId worker
           update (Prepare pid "Continue turn")

      _ -> pure ()

  replaceFee playerId doing total
    | doing > total = pure ()
    | otherwise =
      do playerState <- view (getField (gamePlayer playerId))
         let cubes = getAvailable Cube playerState
             discs = getAvailable Disc playerState
             todo  = 1 + total - doing
             disable n t =
               do let w = Worker { workerOwner = playerId, workerType = t }
                  update (ChangeAvailble w (-n))
                  update (ChangeUnavailable w n)
                  update (Log (Retire w n))

         if | cubes == 0 -> disable todo Disc
            | discs == 0 -> disable todo Cube
            | cubes + discs == todo ->
              do disable cubes Cube
                 disable discs Disc
            | otherwise ->
              do ~(ChActiveWorker ch) <-
                     choose playerId
                       [ ( ChActiveWorker t
                         , "Replace cost " <> showText doing <> "/"
                                           <> showText total
                         ) | t <- enumAll ]
                 disable 1 ch
                 replaceFee playerId (doing + 1) total

  placeOpts edgeId workerT =
    do board <- view (getField gameBoard)
       let accessible newEdgeProv =
             case newEdgeProv of
                Nothing -> True
                Just p  -> edgeProvince edgeId board == Just p
       pure (replaceTargets board accessible edgeId workerT)



  otherPlayerMoveAndPlace edgeId worker =
    do tgts <- placeOpts edgeId (workerType worker)
       update (Prepare (workerOwner worker) "Your worker was replaced")
       ~(ChEdgeEmpty tgtEdgeId spot _) <-
            choose (workerOwner worker)
              [ (ch, "Location for replaced worker") | ch <- tgts ]
       update RemoveWokerFromHand
       update (PlaceWorkerOnEdge tgtEdgeId spot worker)
       update (Log (MoveWorkerTo tgtEdgeId spot worker))
       placeExtra (workerOwner worker) edgeId
                                  1 (replacementCost (workerType worker))

  placeExtra playerId edgeId placing total
    | placing > total = pure ()
    | otherwise =
      do playerState <- view (getField (gamePlayer playerId))
         let optsFor t =
               if getUnavailable t playerState > 0
                  then do os <- placeOpts edgeId t
                          pure [ (playerId :-> o,
                                 "Place bonus worker " <> showText placing
                                              <> "/" <> showText total
                               , do let w = Worker { workerOwner = playerId
                                                   , workerType = t
                                                   }
                                    update (ChangeUnavailable w (-1))
                                    update (PlaceWorkerOnEdge eId spot w)
                                    update (Log (PlaceWorker w eId spot))
                                    placeExtra playerId edgeId (placing+1) total
                                ) | o@(ChEdgeEmpty eId spot _) <- os ]
                  else pure []

         let workerT = getWorkerPreference playerState
             otherT  = otherType workerT
         prefTgts  <- optsFor workerT
         otherTgts <- optsFor otherT
         case (prefTgts,otherTgts) of
           ([],[]) -> placeExtraActive playerId edgeId placing total
           (xs,[]) -> askInputs xs
           ([],ys) -> askInputs ys
           (xs,_)  -> askInputs (changePref : xs)
              where changePref = ( playerId :-> ChSetPreference otherT
                                 , "Change preference to " <> jsKey otherT
                                 , do update (SetWorkerPreference
                                                Worker { workerOwner = playerId
                                                       , workerType = otherT
                                                       })
                                      placeExtra playerId edgeId placing total
                                 )



  placeExtraActive _ _ _ _ = pure () -- XXX




