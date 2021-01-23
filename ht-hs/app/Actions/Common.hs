module Actions.Common where

import Data.Text(Text)
import qualified Data.Map as Map
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

evLog :: [EventElement] -> Interact ()
evLog = update . Log . EvSay


doUpgrade :: PlayerId -> Player -> Stat -> Interact ()
doUpgrade playerId player stat =
  do update (Upgrade playerId stat)
     let worker = Worker { owner = playerId, shape = statWorker stat }
     update (ChangeAvailble worker 1)
     evLog [ "Upgraded ", EvStat stat ]
     when (stat == Actions)
       do let lvl = getLevel Actions player
              diff = actionLimit (lvl+1) - actionLimit lvl
          when (diff > 0) $
             update (ChangeActionLimit diff)


placeSpots ::
  WorkerHome ->
  Player ->
  Text ->
  (WorkerType -> [Choice]) ->
  ( [(Choice,Text)], Bool )
placeSpots home playerState help getSpots =
  ( map disambig spots
  , any isAmbig  spots
  )
  where
  workerPref = getWorkerPreference playerState

  disambig xs =
    case xs of
      [a] -> snd a
      _   -> head [ b | (t,b) <- xs, t == workerPref ]

  isAmbig xs =
    case xs of
      _ : _ : _ -> True
      _         -> False

  spots =
    Map.elems $
    Map.fromListWith (++)
    do t <- enumAll
       guard (getWorker home t playerState > 0)
       x <- getSpots t
       key <- case x of
                ChEdgeEmpty e s _  -> [(e,s)]
                ChEdgeFull e s _ _ -> [(e,s)]
                _ -> []
       pure (key, [(t,(x,help))])



changePref :: Player -> Bool -> [(Choice,Text)]
changePref playerState yes =
  do guard yes
     let otherT = otherType (getWorkerPreference playerState)
         help   = "Change preference to " <> jsKey otherT
     pure (ChSetPreference otherT, help)



