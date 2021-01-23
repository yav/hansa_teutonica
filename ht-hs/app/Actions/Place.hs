module Actions.Place where

import qualified Data.Map as Map
import Control.Monad(guard)

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Player
import Board
import Question
import Game
import Turn
import Event

import Actions.Common

tryPlace :: PlayerOptions
tryPlace state =
  do (turn,playerState) <- startAction state
     let workerT      = getWorkerPreference playerState

         board        = getField gameBoard state
         player       = currentPlayer turn

         gateways     = accessibleProvinces player (usedGateways turn) board
         accessible   = maybe True (`Map.member` gateways)
         gatewayFor edgeId =
                        (`Map.lookup` gateways) =<< edgeProvince edgeId board

         totWorkers   = sum (map (\t -> getWorker Active t playerState) enumAll)
         canReplace w = owner w /= player &&
                        totWorkers > replacementCost (shape w)

         disambig     = map snd . Map.elems . Map.fromListWith pickPref
           where pickPref a@(t1,_) b = if t1 == workerT then a else b

         getFree      = disambig
                      $ do t <- enumAll
                           guard (getWorker Active t playerState > 0)
                           x <- freeSpots board accessible t
                           let ChEdgeEmpty e s _ = x
                           pure ((e,s), (t,(x,"Place a worker")))
         getFull      = disambig
                      $ do t <- enumAll
                           x <- replaceSpots board accessible t canReplace
                           let ChEdgeFull e s _ _ = x
                           pure ((e,s), (t,(x,"Replace a worker")))
         changePref   = do let otherT = otherType workerT
                           guard (getWorker Active otherT playerState > 0)
                           let help = "Change preference to " <>
                                      jsKey otherT
                           pure (ChSetPreference otherT, help)

     (ch,help) <- [ (player :-> c,help) |
                     (c,help) <- changePref ++ getFree ++ getFull ]
     pure (ch, help, handleChoice gatewayFor ch)

  where
  handleChoice gatewayFor (pid :-> ch) =
    case ch of
      ChSetPreference t ->
        do let w = Worker { owner = pid, shape = t }
           update (SetWorkerPreference w)

      ChEdgeEmpty edgeId spot workerT ->
        doAction
        do let w = Worker { owner = pid, shape = workerT }
           update (ChangeAvailble w (-1))
           case gatewayFor edgeId of
             Just g -> update (UseGateway g)
             _      -> pure ()
           update (PlaceWorkerOnEdge edgeId spot w)
           update (Log (EvSay ["Placed ", EvWorker w, " on "
                                       , EvEdge edgeId (Just spot)]))

      ChEdgeFull edgeId spot ~(Just workerT) worker ->
        doAction
        do board <- view (getField gameBoard)
           update (RemoveWorkerFromEdge edgeId spot)
           update (AddWorkerToHand (edgeProvince edgeId board) worker)
           let ourWorker = Worker { owner = pid, shape = workerT }
           update (ChangeAvailble ourWorker (-1))
           update (PlaceWorkerOnEdge edgeId spot ourWorker)
           replaceFee pid 1 (replacementCost (shape worker))
           evLog [ "Replaced ", EvWorker worker, " with ",
                   EvWorker ourWorker, " on ", EvEdge edgeId (Just spot) ]
           otherPlayerMoveAndPlace edgeId worker
           update (Prepare pid "Continue turn")

      _ -> pure ()

  replaceFee playerId doing total
    | doing > total = pure ()
    | otherwise =
      do playerState <- view (getField (gamePlayer playerId))
         let cubes = getWorker Active Cube playerState
             discs = getWorker Active Disc playerState
             todo  = 1 + total - doing
             disable n t =
               do let w = Worker { owner = playerId, shape = t }
                  update (ChangeAvailble w (-n))
                  update (ChangeUnavailable w n)
                  evLog [ "Retired ", EvInt n, " ", EvWorker w ]

         if | cubes == 0 -> disable todo Disc
            | discs == 0 -> disable todo Cube
            | cubes + discs == todo ->
              do disable cubes Cube
                 disable discs Disc
            | otherwise ->
              do ~(ChActiveWorker ch) <-
                     choose playerId
                       [ ( ChActiveWorker t
                         , "Replace cost " <> showText doing <> "/"
                                           <> showText total
                         ) | t <- enumAll ]
                 disable 1 ch
                 replaceFee playerId (doing + 1) total

  placeOpts edgeId workerT =
    do board <- view (getField gameBoard)
       let accessible newEdgeProv =
             case newEdgeProv of
                Nothing -> True
                Just p  -> edgeProvince edgeId board == Just p
       pure (replaceTargets board accessible edgeId workerT)



  otherPlayerMoveAndPlace edgeId worker =
    do tgts <- placeOpts edgeId (shape worker)
       update (Prepare (owner worker) "Your worker was replaced")
       ~(ChEdgeEmpty tgtEdgeId spot _) <-
            choose (owner worker)
              [ (ch, "Location for replaced worker") | ch <- tgts ]
       update RemoveWokerFromHand
       update (PlaceWorkerOnEdge tgtEdgeId spot worker)
       evLog [ "Moved ", EvWorker worker, " to ", EvEdge tgtEdgeId (Just spot) ]
       placeExtra (owner worker) edgeId
                                  1 (replacementCost (shape worker))


  placeExtra playerId edgeId placing total
    | placing > total = pure ()
    | otherwise =
      do playerState <- view (getField (gamePlayer playerId))

         let passiveOptsFor t
               | getWorker Passive t playerState > 0 =
                  do os <- placeOpts edgeId t
                     pure [ (playerId :-> o,
                            "Place bonus worker " <> showText placing
                                         <> "/" <> showText total
                          , do let w = Worker { owner = playerId
                                              , shape = t
                                              }
                               update (ChangeUnavailable w (-1))
                               update (PlaceWorkerOnEdge eId spot w)
                               evLog [ "Placed ", EvWorker w,
                                       " on ", EvEdge eId (Just spot) ]
                               placeExtra playerId edgeId (placing+1) total
                           ) | o@(ChEdgeEmpty eId spot _) <- os ]
               | otherwise = pure []

         let activeOptsFor t
               | getWorker Active t playerState > 0 =
                  do os <- placeOpts edgeId t
                     pure [ (playerId :-> o,
                            "Place (active) bonus worker " <> showText placing
                                         <> "/" <> showText total
                          , do let w = Worker { owner = playerId
                                              , shape = t
                                              }
                               update (ChangeAvailble w (-1))
                               update (PlaceWorkerOnEdge eId spot w)
                               evLog [ "Placed ", EvWorker w,
                                       " on ", EvEdge eId (Just spot) ]
                               placeExtra playerId edgeId (placing+1) total
                           ) | o@(ChEdgeEmpty eId spot _) <- os ]
               | otherwise = pure []



         let doPlaceExtra optsFor ifNoOpts =
               do let workerT = getWorkerPreference playerState
                      otherT  = otherType workerT
                      giveUp  = ( playerId :-> ChDone "Done"
                                , "Don't place additional workers"
                                , pure ()
                                )

                  prefTgts  <- optsFor workerT
                  otherTgts <- optsFor otherT
                  case (prefTgts,otherTgts) of
                    ([],[]) -> ifNoOpts
                    (xs,[]) -> askInputs (giveUp : xs)
                    ([],ys) -> askInputs (giveUp : ys)
                    (xs,_)  -> askInputs (giveUp : changePref : xs)
                       where
                       changePref =
                         ( playerId :-> ChSetPreference otherT
                         , "Change preference to " <> jsKey otherT
                         , do update (SetWorkerPreference
                                         Worker { owner = playerId
                                                , shape = otherT
                                                })
                              placeExtra playerId edgeId placing total
                         )


         doPlaceExtra passiveOptsFor $
            doPlaceExtra activeOptsFor $
              pure () -- XXX: according to the rules if you have *no*
                      -- workers you should be allowed to move a worker
                      -- on the board (presumably in the same proving,
                      -- or into the capital?)





