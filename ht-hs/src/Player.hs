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
import GHC.Generics

import qualified Data.Aeson as JS

import Common.Utils

import Basics
import Stats
import Bonus


data Player = Player
  { stats         :: Map Stat Level
  , available     :: Map WorkerType Int
  , unavailable   :: Map WorkerType Int
  , bonuses       :: [BonusToken]
  , spentBonuses  :: [BonusToken]
  , points        :: Int
  , preference    :: WorkerType
  } deriving (Show,Generic)

zeroState :: Player
zeroState = Player
  { stats         = Map.fromList [ (stat,1) | stat <- enumAll ] -- XXX
  , available     = Map.fromList [ (w,0)    | w    <- enumAll ]
  , unavailable   = Map.fromList [ (w,0)    | w    <- enumAll ]
  , preference    = Cube

    -- XXX
  , bonuses       = [ BonusAct3,BonusAct3,BonusAct4
                    , BonusSwap,BonusSwap
                    , BonusUpgrade, BonusUpgrade, BonusMove, BonusMove
                    , BonusExtra, BonusExtra
                    ]

  , spentBonuses  = []
  , points        = 0
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
changeAvailable w n = \s -> s { available = Map.adjust (+n) w (available s) }

-- | How many workers of the given type we have.
getAvailable :: WorkerType -> Player -> Int
getAvailable w s = available s Map.! w

-- | Add this many workers to the pool of unavailable worker.
-- Use negative number to decrease the number of workers.
changeUnavailable :: WorkerType -> Int -> Player -> Player
changeUnavailable w n =
  \s -> s { unavailable = Map.adjust (+n) w (unavailable s) }

-- | How many workers of the given type we have.
getUnavailable :: WorkerType -> Player -> Int
getUnavailable w s = unavailable s Map.! w

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
gainBonus b = \s -> s { bonuses = b : bonuses s }

-- | Mark a bonus token as spent.
useBonus :: BonusToken -> Player -> Player
useBonus b = \s ->
  s { bonuses      = List.delete b (bonuses s)
    , spentBonuses = b : spentBonuses s
    }

-- | Get available bonus tokens.
getBonuses :: Player -> [BonusToken]
getBonuses = bonuses

-- | Get spent bonus tokens.
getUsedBonuses :: Player -> [BonusToken]
getUsedBonuses = spentBonuses

--------------------------------------------------------------------------------

-- | Increase a player's state.  Note that this does not give the extra worker.
levelUp :: Stat -> Player -> Player
levelUp stat s = s { stats = Map.adjust (+1) stat (stats s) }

-- | Get the level of the specified stat
getLevel :: Stat -> Player -> Level
getLevel stat s = stats s Map.! stat

--------------------------------------------------------------------------------

-- | Add so many VP to the player.
addVP :: Int -> Player -> Player
addVP n = \s -> s { points = n + points s }

-- | Get the player's VP.
getVP :: Player -> Int
getVP = points

--------------------------------------------------------------------------------

instance JS.ToJSON Player

