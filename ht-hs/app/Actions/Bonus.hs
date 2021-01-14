module Actions.Bonus where

import Control.Monad(guard)

import Common.Basics
import Common.Field
import Common.Interact

import Question
import Bonus
import Player
import Game
import Event
import Board

import Actions.Common


bonusAction :: BonusToken -> PlayerOptions
bonusAction b state =
  do let playerId = gameCurrentPlayer state
         player   = getField (gamePlayer playerId) state
         board    = getField gameBoard state
         opt act  = ( playerId :-> ChBonusToken b, "Use Bonus Token"
                    , do update (Log (UsedBonus b))
                         act :: Interact ()
                         update (UseBonusToken playerId b)
                    )
     guard (b `elem` getBonuses player)
     case b of
       BonusAct3 -> [ opt (update (ChangeActionLimit 3)) ]
       BonusAct4 -> [ opt (update (ChangeActionLimit 4)) ]
       BonusUpgrade -> []
       BonusSwap ->
         [ ( playerId :-> q
           , "Move back using bonus token"
           , do update (UseBonusToken playerId b)
                update (SwapWorkers nodeId spot)
                update (Log (SwappedWorkers nodeId spot b))
           )
         | q@(ChNodeFull nodeId spot) <- swappableOffices playerId board
         ]

       BonusMove -> []
       BonusExtra -> []


