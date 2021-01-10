module Actions.Common where

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




