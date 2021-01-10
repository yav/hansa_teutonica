module Actions where

import Control.Monad(guard)

import Common.Interact
import Common.Field

import Basics
import Stats
import Player
import Question
import Game
import Turn
import Event

import Actions.Common
import Actions.Place
import Actions.Move
import Actions.Hire
import Actions.Complete

nextAction :: Interact ()
nextAction =
  do state <- getState
     -- XXX: check end game
     let normalOpts = tryPlace state ++ tryMove state ++ tryHire state ++
                      tryCompleteEdge state
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
     pure (currentPlayer turn :-> ChDone "End Turn", "End turn", nextTurn)


