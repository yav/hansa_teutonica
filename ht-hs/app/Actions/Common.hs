module Actions.Common where

import Data.Text(Text)
import Control.Monad(guard)

import Common.Interact
import Common.Field

import Basics
import Player
import Question
import Game
import Turn
import Event

type PlayerChoice  = (WithPlayer Choice, Text, Interact ())
type PlayerOptions = Game -> [PlayerChoice]

startAction :: Game -> [(Turn,Player)]
startAction state =
  do let turn = getField gameTurn state
         playerState = getField (gamePlayer (currentPlayer turn)) state
     guard (getField actionsDone turn < getField currentActionLimit turn)
     pure (turn, playerState)

doAction :: Interact () -> Interact ()
doAction act =
  do update (Log StartAction)
     act
     update (ChangeDoneActions 1)
     update (Log EndAction)




