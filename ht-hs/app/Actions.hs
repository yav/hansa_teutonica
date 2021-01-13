module Actions where

import Control.Monad(guard,when)

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Stats
import Player
import Question
import Game
import Turn
import Event
import Board

import Actions.Common
import Actions.Place
import Actions.Move
import Actions.Hire
import Actions.Complete
import Actions.Bonus

nextAction :: Interact ()
nextAction =
  do state <- getState
     -- XXX: check end game
     let normalOpts = tryPlace state ++ tryMove state ++ tryHire state ++
                      tryCompleteEdge state ++
                      [ a | b <- enumAll, a <- bonusAction b state ]
         -- NOTE: this will not catch the corrner case of the player having
         -- active workers, but there being no place on the board for them.
         opts       = tryEndTurn (null normalOpts) state ++ normalOpts
     askInputs opts
     nextAction

nextTurn :: Interact ()
nextTurn =
  do state <- getState
     let turn = getField gameTurn state
         curP = currentPlayer turn
         nextPlayerId = playerAfter curP state
         actLvl = getLevel Actions (getField (gamePlayer nextPlayerId) state)
     update (Log (EndTurn curP))
     update (Log (StartTurn nextPlayerId))
     update (NewTurn (newTurn nextPlayerId actLvl))

tryEndTurn :: Bool -> PlayerOptions
tryEndTurn forceEnd state =
  do let turn = getField gameTurn state
     guard (forceEnd ||
            getField actionsDone turn == getField currentActionLimit turn)
     pure (currentPlayer turn :-> ChDone "End Turn", "End turn",
              do replaceTokens
                 nextTurn)

replaceTokens :: Interact ()
replaceTokens =
  do n <- view (getField gameTokenRemaining)
     ts <- view (getField gameTokens)
     when (n < length ts)
       do p <- view gameCurrentPlayer
          spots <- view (tokenSpots . getField gameBoard)
          let t = head ts
          update (PlacingBonus (Just t))
          ~(ChEdge edgeId) <- choose p [ (spot,"Place token here")
                                                              | spot <- spots ]
          update (PlacingBonus Nothing)
          update (EdgeSetBonus edgeId t)
          update (Log (PlacedBonus edgeId t))
          replaceTokens




