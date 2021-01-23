module Actions.FixedBonus where

import Control.Monad(when)
import Data.Maybe(isJust)

import Common.Basics
import Common.Field
import Common.Interact

import Actions.Common
import Basics
import Question
import Game
import Board
import Event
import Bonus

doFixedBonus :: Bool -> PlayerId -> EdgeId -> FixedBonus -> Interact ()
doFixedBonus early playerId edgeId bonus =
  case bonus of
    BonusPlace2         -> when early $ doPlaceInProvince playerId 1 2
    BonusMove2          -> when (not early) $ bonusMove2 playerId
    BonusGainPrivilege  -> pure ()
    BonusBuildInGreen   -> pure ()
    BonusReuse2         -> pure ()


doPlaceInProvince :: PlayerId -> Int -> Int -> Interact ()
doPlaceInProvince playerId placing limit
  | placing > limit = pure ()
  | otherwise =
    do player <- view (getField (gamePlayer playerId))
       board  <- view (getField gameBoard)
       let (opts,ambig) = placeSpots Active player "Place worker"
                                                        (freeSpots board isJust)
           allOpts = (ChDone "Done", "Don't place additional workers")
                   : changePref player ambig ++ opts
       case opts of
         [] -> pure ()
         _  -> do ch <- choose playerId allOpts
                  case ch of
                    ChDone {} -> pure()
                    ChSetPreference t ->
                       do let w = Worker { owner = playerId, shape = t }
                          update (SetWorkerPreference w)
                          doPlaceInProvince playerId placing limit
                    ~(ChEdgeEmpty edgeId spot t) ->
                        do let w = Worker { owner = playerId, shape = t }
                           update (ChangeAvailble w (-1))
                           update (PlaceWorkerOnEdge edgeId spot w)
                           evLog ["Placed ", EvWorker w, " on "
                                              , EvEdge edgeId (Just spot)]
                           doPlaceInProvince playerId (placing+1) limit

bonusMove2 :: PlayerId -> Interact ()
bonusMove2 playerId =
  askInputs . normalMovePieces playerId 2 True (const True) (==) =<< getState

