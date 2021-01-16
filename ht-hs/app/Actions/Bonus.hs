module Actions.Bonus where

import Control.Monad(guard,unless)

import Common.Utils
import Common.Basics
import Common.Field
import Common.Interact

import Basics
import Question
import Bonus
import Player
import Game
import Event
import Board
import Stats

import Actions.Common


bonusAction :: BonusToken -> PlayerOptions
bonusAction b state =
  do let playerId = gameCurrentPlayer state
         player   = getField (gamePlayer playerId) state
         board    = getField gameBoard state
         opt act  = ( playerId :-> ChBonusToken b, "Use Bonus Token"
                    , do update (Log StartAction)
                         update (Log (UsedBonus b))
                         act :: Interact ()
                         update (UseBonusToken playerId b)
                         update (Log EndAction)
                    )
     guard (b `elem` getBonuses player)
     case b of
       BonusAct3 -> [ opt (update (ChangeActionLimit 3)) ]
       BonusAct4 -> [ opt (update (ChangeActionLimit 4)) ]
       BonusUpgrade ->
         do let stats = [ s | s <- enumAll, getLevel s player < maxStat s ]
            guard (not (null stats))
            pure $ opt $ askInputs [ ( playerId :-> ChUpgrade stat
                                     , "Upgrade " <> jsKey stat
                                     , doUpgrade playerId player stat
                                     ) | stat <- stats
              ]

       BonusSwap ->
         [ ( playerId :-> q
           , "Move back using bonus token"
           , do update (Log StartAction)
                update (UseBonusToken playerId b)
                update (SwapWorkers nodeId spot)
                update (Log (SwappedWorkers nodeId spot b))
                update (Log EndAction)
           )
         | q@(ChNodeFull nodeId spot) <- swappableOffices playerId board
         ]

       BonusMove ->
         do let spots = moveOpponentSpots board playerId
            guard (not (null spots))
            pure $ opt $ doMove 1 3
         where
         doMove n l
           | n > l = pure ()
           | otherwise =
             do board <- view (getField gameBoard)
                let spots = moveOpponentSpots board playerId
                unless (null spots) $
                  askInputs [ ( playerId :-> q
                              , "Pick up " <> showText n <> "/" <> showText l
                              , doPickup n l q
                              ) | q <- spots ]

         doPickup n l ~(ChEdgeFull edgeId spot _ worker) =
           do update (RemoveWorkerFromEdge edgeId spot)
              board <- view (getField gameBoard)
              let prov = edgeProvince edgeId board
              update (AddWorkerToHand prov worker)
              update (Log (PickUp worker edgeId spot))
              let spots = freeSpots board (== prov) (shape worker)
              askInputs [ ( playerId :-> q
                          , "Put down " <> showText n <> "/" <> showText l
                          , doPutDown n l worker q
                          ) | q <- spots ]

         doPutDown n l worker ~(ChEdgeEmpty edgeId spot _) =
           do update RemoveWokerFromHand
              update (PlaceWorkerOnEdge edgeId spot worker)
              update (Log (MoveWorkerTo edgeId spot worker))
              doMove (n+1 :: Int) l


       BonusExtra -> []
