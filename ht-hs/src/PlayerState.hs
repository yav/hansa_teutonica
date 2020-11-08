module PlayerState where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Utils
import Stats
import Bonus


data PlayerState = PlayerState
  { playerStats         :: Map Stat Level
  , availableWorkers    :: Map Worker Int
  , unavailableWorkers  :: Map Worker Int
  , availableBonuses    :: [Bonus]
  , usedBonuses         :: [Bonus]
  } deriving Show

zeroState :: PlayerState
zeroState = PlayerState
  { playerStats         = Map.fromList [ (stat,0) | stat <- enumAll ]
  , availableWorkers    = Map.fromList [ (w,0)    | w    <- enumAll ]
  , unavailableWorkers  = Map.fromList [ (w,0)    | w    <- enumAll ]
  , availableBonuses    = []
  , usedBonuses         = []
  }

initialPlayerState :: Int -> PlayerState
initialPlayerState turnOrder =
     turnOrder `times` hireWorker Cube
  $  7         `times` returnUnavailable Cube
  $ foldr levelUp zeroState enumAll

changeAvailable :: (Int -> Int) -> Worker -> PlayerState -> PlayerState
changeAvailable f w =
  \s -> s { availableWorkers = Map.adjust f w (availableWorkers s) }

changeUnavailable :: (Int -> Int) -> Worker -> PlayerState -> PlayerState
changeUnavailable f w =
  \s -> s { unavailableWorkers = Map.adjust f w (unavailableWorkers s) }

takeAvailable, returnAvailalbe, takeUnavailable, returnUnavailable ::
  Worker -> PlayerState -> PlayerState
takeAvailable     = changeAvailable (subtract 1)
returnAvailalbe   = changeAvailable (+1)
takeUnavailable   = changeUnavailable (subtract 1)
returnUnavailable = changeUnavailable (+1)

hireWorker :: Worker -> PlayerState -> PlayerState
hireWorker w = returnAvailalbe w . takeUnavailable w

useBonus :: Bonus -> PlayerState -> PlayerState
useBonus b s =
  s { availableBonuses = List.delete b (availableBonuses s)
    , usedBonuses      = b : usedBonuses s
    }

levelUp :: Stat -> PlayerState -> PlayerState
levelUp stat = returnAvailalbe (statWorker stat) . bumpLevel
  where
  bumpLevel s = s { playerStats = Map.adjust (+1) stat (playerStats s) }


