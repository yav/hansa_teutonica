module Actions where

import qualified Data.Map as Map
import Control.Monad(guard)
import Data.Text(Text)
import qualified Data.Text as Text

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Stats
import Player
import Board
import Edge
import Question
import Game
import Turn


nextAction :: Interact ()
nextAction =
  do state <- getState
     -- XXX: check end game
     let normalOpts = tryPlace state ++ tryMove state
         -- NOTE: this will not catch the corrner case of the player having
         -- active workers, but there being no place on the board for them.
         opts       = tryEndTurn (null normalOpts) state ++ normalOpts
     askInputs opts
     nextAction

nextTurn :: Interact ()
nextTurn =
  do state <- getState
     let turn = getField gameTurn state
         nextPlayerId = playerAfter (currentPlayer turn) state
         actLvl = getLevel Actions (getField (gamePlayer nextPlayerId) state)
     update (NewTurn (newTurn nextPlayerId actLvl))

-------------------------------------------------------------------------------
startAction :: Game -> [(Turn,Player)]
startAction state =
  do let turn = getField gameTurn state
         playerState = getField (gamePlayer (currentPlayer turn)) state
     guard (getField actionsDone turn < getField currentActionLimit turn)
     pure (turn, playerState)

type PlayerOptions = Game -> [(WithPlayer Choice, Text, Interact ())]

tryEndTurn :: Bool -> PlayerOptions
tryEndTurn forceEnd state =
  do let turn = getField gameTurn state
     guard (forceEnd ||
            getField actionsDone turn == getField currentActionLimit turn)
     pure (currentPlayer turn :-> ChDone "End Turn", "End turn", nextTurn)

tryPlace :: PlayerOptions
tryPlace state =
  do (turn,playerState) <- startAction state
     let workerT      = getWorkerPreference playerState

         board        = getField gameBoard state
         player       = currentPlayer turn

         gateways     = accessibleProvinces player (usedGateways turn) board
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
           -- XXX: use a gateway
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
         player = currentPlayer turn
     guard (limit > 0)
     pickupQuestion (1::Int) player limit pieces
  where
  movablePieces state =
     let board      = getField gameBoard state
         turn       = getField gameTurn state
         player     = currentPlayer turn
         canMove w  = workerOwner w == player
     in pickupSpots board (const True) canMove

  pickupQuestion num player limit opts =
    [ ( player :-> ch
      , "Move worker " <> Text.pack (show num) <> "/" <> Text.pack (show limit)
      , pickup num limit edgeId spot w
      ) | ch@(ChEdgeFull edgeId spot _ w) <- opts ]

  pickup num limit edgeId spot w =
    do update (RemoveWorkerFromEdge edgeId spot)
       prov <- view \g -> edgeProvince (getField gameBoard g) edgeId
       update (AddWorkerToHand prov w)
       opts <- view movablePieces
       if num < limit
         then askInputs $ ( workerOwner w :-> ChDone "Done"
                          , "No more moves"
                          , putDown)
                        : pickupQuestion (num+1) (workerOwner w) limit opts
         else putDown

  putDown =
    do board <- view (getField gameBoard)
       mb    <- view (nextPickedUp . getField gameTurn)
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
     let player     = currentPlayer turn
         limit      = hireLimit (getLevel Hire playerState)
         question t = (player :-> ChPassiveWorker t, "Hire worker", doHire limit t)
     [ question t | t <- enumAll, getUnavailable playerState t > 0 ]
  where
{-
  hire n t =
    do pid <- view (viewTurn currentPlayer
       let w = worker t
       update (ChangeUnavailable w (-n))
       update (ChangeAvailble    w n)
-}

  doHire lim ch = undefined {-
    do state <- getState
       let turn   = gameStatus state
           playerId = currentPlayer turn
           player = viewPlayer playerId id
           cubes  = getUnavailable Cube
           discs  = getUnavailable Disc
           worker t = Worker { workerOwner = playerId, workerType = t }
       case limit of
         Just l
           | cubes == 0 -> hire (min l discs) Disc
           | discs == 0 -> hire (min l cubes) Cube
           | cubes + discs > l ->

         _ -> do hire discs Disc
                 hire cubes 

-}
