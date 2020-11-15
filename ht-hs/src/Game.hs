module Game where

import Data.Map(Map)
-- import qualified Data.Map as Map
import Data.Set(Set)

import Basics
import Bonus
import Player
import Area

data Game = Game
  { gamePlayers     :: Map PlayerColor Player
  , gameTurnOrder   :: [PlayerColor]
  , gameMap         :: Area
  }


data Turn = Turn
  { turnCurrentPlayer :: PlayerColor
  , turnActionNumber  :: Int
  , turnActionLimit   :: Int
  , turnForeignUsed   :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  }


