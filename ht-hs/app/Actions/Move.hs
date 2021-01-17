module Actions.Move where

import Control.Monad(guard)
import qualified Data.Text as Text

import Common.Interact
import Common.Field

import Basics
import Stats
import Player
import Board
import Question
import Game
import Turn
import Event

import Actions.Common



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
         canMove w  = owner w == player
     in pickupSpots board (const True) canMove

  pickupQuestion num player limit opts =
    [ ( player :-> ch
      , "Move worker " <> Text.pack (show num) <> "/" <> Text.pack (show limit)
      , (if num == 1 then doAction else id) (pickup num limit edgeId spot w)
      ) | ch@(ChEdgeFull edgeId spot _ w) <- opts ]

  pickup num limit edgeId spot w =
    do update (RemoveWorkerFromEdge edgeId spot)
       prov <- view \g -> edgeProvince edgeId (getField gameBoard g)
       update (AddWorkerToHand prov w)
       evLog [ "Picked up ", EvWorker w, " from ", EvEdge edgeId (Just spot) ]
       opts <- view movablePieces
       if num < limit
         then askInputs $ ( owner w :-> ChDone "Done"
                          , "No more moves"
                          , putDown)
                        : pickupQuestion (num+1) (owner w) limit opts
         else putDown

  putDown =
    do board <- view (getField gameBoard)
       mb    <- view (nextPickedUp . getField gameTurn)
       case mb of
         Nothing -> pure ()
         Just (thisProv,w) ->
           do let accessible prov = prov == Nothing || prov == thisProv
              ~(ChEdgeEmpty tgtEdge tgtSpot _) <-
                     choose (owner w)
                         do x <- freeSpots board accessible (shape w)
                            pure (x, "New worker location")

              update RemoveWokerFromHand
              update (PlaceWorkerOnEdge tgtEdge tgtSpot w)
              evLog [ "Moved ", EvWorker w, " to ",
                                              EvEdge tgtEdge (Just tgtSpot) ]
              putDown



