module Game where

import Data.Map(Map)
import qualified Data.Map as Map
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

getPlayer :: Game -> PlayerColor -> Player
getPlayer Game { gamePlayers } p = gamePlayers Map.! p

data Turn = Turn
  { turnCurrentPlayer :: PlayerColor
  , turnActionNumber  :: Int
  , turnActionLimit   :: Int
  , turnForeignUsed   :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  }

