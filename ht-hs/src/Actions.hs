module Actions where

import qualified Data.Map as Map
import Control.Monad(guard)

import Utils
import Basics
import Player
import Board
import Edge
import Question
import Game
import Interact


startGame ::
  GameState NoUpdates -> (InteractState NoUpdates, [WithPlayer OutMsg])
startGame g = interaction nextAction (startState g)


nextAction :: Interact ()
nextAction =
  do state <- inGame getState
     let opts = tryPlace state
     askInputs opts
     nextAction


--------------------------------------------------------------------------------
startAction :: GameState NoUpdates -> [(Turn,Player)]
startAction state =
  do GameInProgress turn <- pure (gameStatus state)
     let playerState = gamePlayers state Map.! turnCurrentPlayer turn
     guard (turnActionsDone turn < turnActionLimit turn)
     pure (turn, playerState)


tryPlace :: GameState NoUpdates -> [(WithPlayer Choice, Interact ())]
tryPlace state =
  do (turn,playerState) <- startAction state
     let workerT      = getWorkerPreference playerState

         board        = gameBoard state
         player       = turnCurrentPlayer turn

         gateways     = accessibleProvinces player (turnUsedGateways turn) board
         accessible   = maybe True (`Map.member` gateways)

         totWorkers   = sum (map (getAvailable playerState) enumAll)
         canReplace w = workerOwner w /= player &&
                        totWorkers > replacementCost (workerType w)

         getFree      = do guard (getAvailable playerState workerT > 0)
                           freeSpots board accessible workerT
         getFull      = occupiedSpots board accessible workerT canReplace
         changePref   = do let otherT = otherType workerT
                           guard (getAvailable playerState otherT > 0)
                           pure (ChSetPreference otherT)

     ch <- [ player :-> c | c <- changePref ++ getFree ++ getFull ]
     pure (ch, handleChoice ch)

  where
  handleChoice (pid :-> ch) =
    case ch of
      ChSetPreference t ->
        do let w = Worker { workerOwner = pid, workerType = t }
           update (SetWorkerPreference w)

      ChEdge edgeId spot workerT mbW ->
        case mbW of
          Nothing ->
            do let w = Worker { workerOwner = pid, workerType = workerT }
               update (ChangeAvailble w (-1))
               update (PlaceWorkerOnEdge edgeId spot w)
               update (ChangeDoneActions 1)

          Just w -> error "XXX: place full"

      _ -> pure ()


