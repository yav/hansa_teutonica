module Actions.Place where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)
import Control.Monad(guard)

import Utils
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
             not (null opts)
           )
     pure Question { qAnswers = [(ChActiveWorker workerT, "Plase on board")]
                   , qContinue =
                       \_ -> Right Ask { askPlayer = pColor
                                       , askQuestion =
                                           Question
                                             { qAnswers  = opts
                                             , qContinue = chooseSpot
                                             }
                                       }
                   }
  where
  game        = turnGame turn
  pColor      = turnCurrentPlayer turn
  player      = getPlayer game pColor
  board       = gameBoard game
  gateways    = accessibleProvinces pColor (turnUsedGateways turn) board
  accessible  = maybe True (`Map.member` gateways)
  totActive   = sum (map (`getAvailable` player) enumAll)
  canReplace worker = workerOwner worker /= pColor &&
                      totActive > replacementCost (workerType worker)


  opts = map placeHelp (freeSpots board accessible workerT) ++
         map replaceHelp (occupiedSpots board accessible workerT canReplace)
  placeHelp a   = (a, "Place worker here")
  replaceHelp a = (a, "Replace this worker")


  chooseSpot ~(ChEdge edgeId spot mb) =
    case mb of
      Nothing ->
        Left turn
          { turnActionsDone = turnActionsDone turn + 1
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
              in game
                   { gameBoard = newBoard
                   , gamePlayers =
                       Map.insert pColor newPlayer (gamePlayers game)
                   }
          }




