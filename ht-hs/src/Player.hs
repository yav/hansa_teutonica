module Player
  ( 
    -- * Initialization
    Player
  , initialPlayer


    -- * Workers
  , changeAvailable
  , getAvailable
  , changeUnavailable
  , getUnavailable
  , hireWorker

    -- * Bonuses
  , gainBonus
  , useBonus
  , getBonuses
  , getUsedBonuses

    -- * Stats
  , levelUp
  , getLevel

    -- * Points
  , addVP
  , getVP
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Basics
import Utils
import Stats
import Bonus


data Player = Player
  { playerStats         :: Map Stat Level
  , availableWorkers    :: Map WorkerType Int
  , unavailableWorkers  :: Map WorkerType Int
  , availableBonuses    :: [BonusToken]
  , usedBonuses         :: [BonusToken]
  , points              :: Int
  } deriving Show

zeroState :: Player
zeroState = Player
  { playerStats         = Map.fromList [ (stat,0) | stat <- enumAll ]
  , availableWorkers    = Map.fromList [ (w,0)    | w    <- enumAll ]
  , unavailableWorkers  = Map.fromList [ (w,0)    | w    <- enumAll ]
  , availableBonuses    = []
  , usedBonuses         = []
  , points              = 0
  }


-- 0 based turn order
initialPlayer :: Int -> Player
initialPlayer turnOrder =
     hireWorker (turnOrder + 1) Cube
  $  changeUnavailable 7 Cube
  $ foldr levelUp zeroState enumAll



--------------------------------------------------------------------------------
changeAvailable :: Int -> WorkerType -> Player -> Player
changeAvailable n w =
  \s -> s { availableWorkers = Map.adjust (+n) w (availableWorkers s) }

getAvailable :: WorkerType -> Player -> Int
getAvailable w = \s -> availableWorkers s Map.! w

changeUnavailable :: Int -> WorkerType -> Player -> Player
changeUnavailable n w =
  \s -> s { unavailableWorkers = Map.adjust (+n) w (unavailableWorkers s) }

getUnavailable :: WorkerType -> Player -> Int
getUnavailable w = \s -> unavailableWorkers s Map.! w

hireWorker :: Int -> WorkerType -> Player -> Player
hireWorker n w = changeAvailable n w . changeUnavailable (-n) w


--------------------------------------------------------------------------------
gainBonus :: BonusToken -> Player -> Player
gainBonus b = \s -> s { availableBonuses = b : availableBonuses s }

useBonus :: BonusToken -> Player -> Player
useBonus b = \s ->
  s { availableBonuses = List.delete b (availableBonuses s)
    , usedBonuses      = b : usedBonuses s
    }

getBonuses :: Player -> [BonusToken]
getBonuses = availableBonuses

getUsedBonuses :: Player -> [BonusToken]
getUsedBonuses = usedBonuses

--------------------------------------------------------------------------------
levelUp :: Stat -> Player -> Player
levelUp stat = changeAvailable 1 (statWorker stat) . bumpLevel
  where
  bumpLevel s = s { playerStats = Map.adjust (+1) stat (playerStats s) }

getLevel :: Stat -> Player -> Level
getLevel stat = \s -> playerStats s Map.! stat

--------------------------------------------------------------------------------

addVP :: Int -> Player -> Player
addVP n = \s -> s { points = n + points s }

getVP :: Player -> Int
getVP = points
