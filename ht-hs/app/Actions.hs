module Actions where

import qualified Data.Map as Map
import Control.Monad(guard)
import Data.Text(Text)
import qualified Data.Text as Text

import Common.Utils
import Common.Interact

import Basics
import Stats
import Player
import Board
import Edge
import Question
import Game


nextAction :: Interact ()
nextAction =
  do state <- getGameState
     -- XXX: check end game
     let normalOpts = tryPlace state ++ tryMove state
         -- NOTE: this will not catch the corrner case of the player having
         -- active workers, but there being no place on the board for them.
         opts       = tryEndTurn (null normalOpts) state ++ normalOpts
     askInputs opts
     nextAction

nextTurn :: Interact ()
nextTurn =
  do state <- getGameState
     let turn = gameStatus state
         nextPlayerId = playerAfter (turnCurrentPlayer turn) state
         actLvl = viewPlayer nextPlayerId (getLevel Actions) state
     update (NewTurn (newTurn nextPlayerId actLvl))

-------------------------------------------------------------------------------
startAction :: Game -> [(Turn,Player)]
startAction state =
  do let turn = gameStatus state
         playerState = gamePlayers state Map.! turnCurrentPlayer turn
     guard (turnActionsDone turn < turnActionLimit turn)
     pure (turn, playerState)

type PlayerOptions = Game -> [(WithPlayer Choice, Text, Interact ())]

tryEndTurn :: Bool -> PlayerOptions
tryEndTurn forceEnd state =
  do let turn = gameStatus state
     guard (forceEnd || turnActionsDone turn == turnActionLimit turn)
     pure (turnCurrentPlayer turn :-> ChDone "End Turn", "End turn", nextTurn)

tryPlace :: PlayerOptions
tryPlace state =
  do (turn,playerState) <- startAction state
     let workerT      = getWorkerPreference playerState

         board        = gameBoard state
         player       = turnCurrentPlayer turn

         gateways     = accessibleProvinces player (turnUsedGateways turn) board
         accessible   = maybe True (`Map.member` gateways)

         totWorkers   = sum (map (getAvailable playerState) enumAll)
         canReplace w = workerOwner w /= player &&
                        totWorkers > replacementCost (workerType w)

         getFree      = do guard (getAvailable playerState workerT > 0)
                           x <- freeSpots board accessible workerT
                           pure (x, "Place a worker")
         getFull      = do x <- replaceSpots board accessible workerT canReplace
                           pure (x, "Replace a worker")
         changePref   = do let otherT = otherType workerT
                           guard (getAvailable playerState otherT > 0)
                           let help = "Change preference to " <>
                                      workerTypeToKey otherT
                           pure (ChSetPreference otherT, help)

     (ch,help) <- [ (player :-> c,help) |
                     (c,help) <- changePref ++ getFree ++ getFull ]
     pure (ch, help, handleChoice ch)

  where
  handleChoice (pid :-> ch) =
    case ch of
      ChSetPreference t ->
        do let w = Worker { workerOwner = pid, workerType = t }
           update (SetWorkerPreference w)

      ChEdgeEmpty edgeId spot workerT ->
        do let w = Worker { workerOwner = pid, workerType = workerT }
           update (ChangeAvailble w (-1))
           update (PlaceWorkerOnEdge edgeId spot w)
           update (ChangeDoneActions 1)

      ChEdgeFull {} -> error "XXX: place full"

      _ -> pure ()



tryMove :: PlayerOptions
tryMove state0 =
  do (turn,playerState) <- startAction state0
     let pieces = movablePieces state0
         limit  = min (movementLimit (getLevel Movement playerState))
                                                          (length pieces)
         player = turnCurrentPlayer turn
     guard (limit > 0)
     pickupQuestion (1::Int) player limit pieces
  where
  movablePieces state =
     let board      = gameBoard state
         turn       = gameStatus state
         player     = turnCurrentPlayer turn
         canMove w  = workerOwner w == player
     in pickupSpots board (const True) canMove

  pickupQuestion num player limit opts =
    [ ( player :-> ch
      , "Move worker " <> Text.pack (show num) <> "/" <> Text.pack (show limit)
      , pickup num limit edgeId spot w
      ) | ch@(ChEdgeFull edgeId spot _ w) <- opts ]

  pickup num limit edgeId spot w =
    do update (RemoveWorkerFromEdge edgeId spot)
       prov <- view \g -> edgeProvince (gameBoard g) edgeId
       update (AddWorkerToHand prov w)
       opts <- view movablePieces
       if num < limit
         then askInputs $ ( workerOwner w :-> ChDone "Done"
                          , "No more moves"
                          , putDown)
                        : pickupQuestion (num+1) (workerOwner w) limit opts
         else putDown

  putDown =
    do board <- view gameBoard
       mb    <- view (viewTurn nextPickedUp)
       case mb of
         Nothing -> update (ChangeDoneActions 1)
         Just (thisProv,w) ->
           do let accessible prov = prov == Nothing || prov == thisProv
              ~(ChEdgeEmpty tgtEdge tgtSpot _) <-
                     choose (workerOwner w)
                         do x <- freeSpots board accessible (workerType w)
                            pure (x, "New worker location")

              update RemoveWokerFromHand
              update (PlaceWorkerOnEdge tgtEdge tgtSpot w)
              putDown



tryHire :: PlayerOptions
tryHire state0 =
  do (turn,playerState) <- startAction state0
     let player = turnCurrentPlayer turn
         question t = (player :-> ChPassiveWorker t, "Hire worker", doHire 0 t)
     [ question t | t <- enumAll, getUnavailable playerState t > 0 ]
  where
  doHire done t = undefined


