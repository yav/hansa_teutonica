module PlayerState
  ( 
    -- * Initialization
    PlayerState
  , initialPlayerState


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

import Utils
import Stats
import Bonus


data PlayerState = PlayerState
  { playerStats         :: Map Stat Level
  , availableWorkers    :: Map WorkerType Int
  , unavailableWorkers  :: Map WorkerType Int
  , availableBonuses    :: [Bonus]
  , usedBonuses         :: [Bonus]
  , points              :: Int
  } deriving Show

zeroState :: PlayerState
zeroState = PlayerState
  { playerStats         = Map.fromList [ (stat,0) | stat <- enumAll ]
  , availableWorkers    = Map.fromList [ (w,0)    | w    <- enumAll ]
  , unavailableWorkers  = Map.fromList [ (w,0)    | w    <- enumAll ]
  , availableBonuses    = []
  , usedBonuses         = []
  , points              = 0
  }


-- 0 based turn order
initialPlayerState :: Int -> PlayerState
initialPlayerState turnOrder =
     hireWorker (turnOrder + 1) Cube
  $  changeUnavailable 7 Cube
  $ foldr levelUp zeroState enumAll



--------------------------------------------------------------------------------
changeAvailable :: Int -> WorkerType -> PlayerState -> PlayerState
changeAvailable n w =
  \s -> s { availableWorkers = Map.adjust (+n) w (availableWorkers s) }

getAvailable :: WorkerType -> PlayerState -> Int
getAvailable w = \s -> availableWorkers s Map.! w

changeUnavailable :: Int -> WorkerType -> PlayerState -> PlayerState
changeUnavailable n w =
  \s -> s { unavailableWorkers = Map.adjust (+n) w (unavailableWorkers s) }

getUnavailable :: WorkerType -> PlayerState -> Int
getUnavailable w = \s -> unavailableWorkers s Map.! w

hireWorker :: Int -> WorkerType -> PlayerState -> PlayerState
hireWorker n w = changeAvailable n w . changeUnavailable (-n) w


--------------------------------------------------------------------------------
gainBonus :: Bonus -> PlayerState -> PlayerState
gainBonus b = \s -> s { availableBonuses = b : availableBonuses s }

useBonus :: Bonus -> PlayerState -> PlayerState
useBonus b = \s ->
  s { availableBonuses = List.delete b (availableBonuses s)
    , usedBonuses      = b : usedBonuses s
    }

getBonuses :: PlayerState -> [Bonus]
getBonuses = availableBonuses

getUsedBonuses :: PlayerState -> [Bonus]
getUsedBonuses = usedBonuses

--------------------------------------------------------------------------------
levelUp :: Stat -> PlayerState -> PlayerState
levelUp stat = changeAvailable 1 (statWorker stat) . bumpLevel
  where
  bumpLevel s = s { playerStats = Map.adjust (+1) stat (playerStats s) }

getLevel :: Stat -> PlayerState -> Level
getLevel stat = \s -> playerStats s Map.! stat

--------------------------------------------------------------------------------

addVP :: Int -> PlayerState -> PlayerState
addVP n = \s -> s { points = n + points s }

getVP :: PlayerState -> Int
getVP = points
