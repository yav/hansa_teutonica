module Actions.Common where

import Data.Text(Text)
import Control.Monad(guard,when)

import Common.Interact
import Common.Field

import Basics
import Stats
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


doUpgrade :: PlayerId -> Player -> Stat -> Interact ()
doUpgrade playerId player stat =
  do update (Upgrade playerId stat)
     let worker = Worker { workerOwner = playerId
                         , workerType = statWorker stat }
     update (ChangeAvailble worker 1)
     update (Log (Upgraded playerId stat))
     when (stat == Actions)
       do let lvl = getLevel Actions player
              diff = actionLimit (lvl+1) - actionLimit lvl
          when (diff > 0) $
             update (ChangeActionLimit diff)
 
