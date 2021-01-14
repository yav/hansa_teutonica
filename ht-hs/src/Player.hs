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
  , setWorkerPreference
  , getWorkerPreference

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

import qualified Data.Aeson as JS
import Data.Aeson ((.=))

import Common.Utils

import Basics
import Stats
import Bonus


data Player = Player
  { playerStats         :: Map Stat Level
  , availableWorkers    :: Map WorkerType Int
  , unavailableWorkers  :: Map WorkerType Int
  , availableBonuses    :: [BonusToken]
  , usedBonuses         :: [BonusToken]
  , points              :: Int
  , preference          :: WorkerType
  } deriving (Read,Show)

zeroState :: Player
zeroState = Player
  { playerStats         = Map.fromList [ (stat,1) | stat <- enumAll ] -- XXX
  , availableWorkers    = Map.fromList [ (w,0)    | w    <- enumAll ]
  , unavailableWorkers  = Map.fromList [ (w,0)    | w    <- enumAll ]

  , availableBonuses    = [BonusAct3,BonusAct3,BonusAct4,BonusSwap,BonusSwap]
  , usedBonuses         = []
  , points              = 0
  , preference          = Cube
  }


-- 0 based turn order
initialPlayer :: Int -> Player
initialPlayer turnOrder =
     hireWorker Cube (turnOrder + 1)
  $  changeUnavailable Cube 7
  $ foldr levelUp' zeroState enumAll
  where
  levelUp' stat = changeAvailable (statWorker stat) 1 . levelUp stat


--------------------------------------------------------------------------------

-- | Add this many workers to the pool of available worker.
-- Use negative number to decrease the number of workers.
changeAvailable :: WorkerType -> Int -> Player -> Player
changeAvailable w n =
  \s -> s { availableWorkers = Map.adjust (+n) w (availableWorkers s) }

-- | How many workers of the given type we have.
getAvailable :: WorkerType -> Player -> Int
getAvailable w s = availableWorkers s Map.! w

-- | Add this many workers to the pool of unavailable worker.
-- Use negative number to decrease the number of workers.
changeUnavailable :: WorkerType -> Int -> Player -> Player
changeUnavailable w n =
  \s -> s { unavailableWorkers = Map.adjust (+n) w (unavailableWorkers s) }

-- | How many workers of the given type we have.
getUnavailable :: WorkerType -> Player -> Int
getUnavailable w s = unavailableWorkers s Map.! w

-- | Move the given number of workers from unavailable to available.
hireWorker :: WorkerType -> Int -> Player -> Player
hireWorker w n = changeAvailable w n . changeUnavailable w (-n)

-- | Prefer placing this worker
getWorkerPreference :: Player -> WorkerType
getWorkerPreference = preference

-- | Set worker preference to this
setWorkerPreference :: WorkerType -> Player -> Player
setWorkerPreference wt = \s -> s { preference = wt }

--------------------------------------------------------------------------------

-- | Add a bonus token to the player.
gainBonus :: BonusToken -> Player -> Player
gainBonus b = \s -> s { availableBonuses = b : availableBonuses s }

-- | Mark a bonus token as spent.
useBonus :: BonusToken -> Player -> Player
useBonus b = \s ->
  s { availableBonuses = List.delete b (availableBonuses s)
    , usedBonuses      = b : usedBonuses s
    }

-- | Get available bonus tokens.
getBonuses :: Player -> [BonusToken]
getBonuses = availableBonuses

-- | Get spent bonus tokens.
getUsedBonuses :: Player -> [BonusToken]
getUsedBonuses = usedBonuses

--------------------------------------------------------------------------------

-- | Increase a player's state.  Note that this does not give the extra worker.
levelUp :: Stat -> Player -> Player
levelUp stat s = s { playerStats = Map.adjust (+1) stat (playerStats s) }

-- | Get the level of the specified stat
getLevel :: Stat -> Player -> Level
getLevel stat s = playerStats s Map.! stat

--------------------------------------------------------------------------------

-- | Add so many VP to the player.
addVP :: Int -> Player -> Player
addVP n = \s -> s { points = n + points s }

-- | Get the player's VP.
getVP :: Player -> Int
getVP = points

--------------------------------------------------------------------------------

instance JS.ToJSON Player where
  toJSON p = JS.object $
    [ jsKey s .= v | (s,v) <- Map.toList (playerStats p) ] ++
    [ "available"     .= jsMap (availableWorkers p)
    , "unavailable"   .= jsMap (unavailableWorkers p)
    , "vp"            .= points p
    , "bonuses"       .= bonusObj
    , "spentBonuses"  .= length (usedBonuses p)
    , "preference"    .= preference p
    ]


    where
    usedBonusMap = Map.fromListWith (+) [ (b,1::Int) | b <- availableBonuses p ]
    bonusObj = JS.object [bonusAsKey b .= v | (b,v) <- Map.toList usedBonusMap]

