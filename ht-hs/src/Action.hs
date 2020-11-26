module Action where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)
import Control.Monad(guard)

import Basics
import Player
import Board
import Edge
import Question
import Game

tryPlace :: Turn -> WorkerType -> Maybe Question
tryPlace turn workerT =
  do guard ( turnActionsDone turn < turnActionLimit turn &&
             getAvailable workerT player > 0 &&
             not (null placeOpts)
           )
     pure Question { qAnswers = [ChActiveWorker workerT]
                   , qContinue = askWhere
                   }
  where
  game        = turnGame turn
  pColor      = turnCurrentPlayer turn
  player      = getPlayer game pColor
  board       = gameBoard game
  gateways    = accessibleProvinces pColor (turnUsedGateways turn) board
  accessible  = maybe True (`Map.member` gateways)
  placeOpts   = freeSpots board accessible workerT

  askWhere _ = Right Ask { askPlayer = pColor
                         , askQuestion =
                             Question
                               { qAnswers = placeOpts
                               , qContinue = doPlace
                               }
                         }

  doPlace ~(ChEdge edgeId spot _) =
    Left turn { turnActionsDone = turnActionsDone turn + 1
              , turnUsedGateways =
                fromMaybe (turnUsedGateways turn)
                do provinceId <- edgeProvince board edgeId
                   gateway    <- Map.lookup provinceId gateways
                   pure (Set.insert gateway (turnUsedGateways turn))
              , turnGame =
                  let updEdge = edgeAddWorker spot
                                  Worker { workerOwner = pColor
                                         , workerType = workerT
                                         }
                      newBoard  = modifyEdge edgeId updEdge board
                      newPlayer = changeAvailable (-1) workerT player
                  in game { gameBoard = newBoard
                          , gamePlayers = Map.insert pColor newPlayer (gamePlayers game)
                          }
              }


