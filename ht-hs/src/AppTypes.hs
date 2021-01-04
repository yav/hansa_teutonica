module AppTypes where

import Game(Game,GameUpdate)
import qualified Game
import Question(Choice)

type State  = Game
type Update = GameUpdate
type Input  = Choice

doUpdate :: Update -> State -> State
doUpdate = Game.doUpdate

