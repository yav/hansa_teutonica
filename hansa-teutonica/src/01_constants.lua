local version = 3       -- bump this to invalidate saves
local version_min = 3   -- bump this for fixes that don't affect saves

-- Board info
local boardPieceZ = 1.5
local landscape = 1
local portrait = 2

local menuLoc = { {-15,14}, {5,13} }
local undoLoc = { {-23,17}, {5,-15} }

-- Type Shape
local trader = 1
local merchant = 2

-- Skills
local actionLevelMap    = { 2, 3, 3, 4, 4, 5 }
local bagLevelMap       = { 3, 5, 7, 50 }
local keyLevelMap       = { 1, 2, 2, 3, 4, }
local bookLevelMap      = { 2, 3, 4, 5, }
local buildingLevelMap  = { "[ffffff]###[-]white[ffffff]###[-]"
                          , "[ff9900]###[-]orange[ff9900]###[-]"
                          , "[ff00ff]###[-]pink[ff00ff]###[-]"
                          , "[333333]###[-]black[333333]###[-]"
                          }

-- Stop types
local stopRoad = 1
local stopShip = 2

-- City Action Type
local upgradeAction   = 1
local upgradeBook     = 2
local upgradeKey      = 3
local upgradeBag      = 4
local upgradeBuilding = 5
local invest          = 6

local endGameInvestPoints = { 7,8,9,11 }
local endGameBonusPoints = { 1, 3, 3, 6, 6, 10, 10, 15, 15, 21 }
                          -- use last one for more

local raceAward = { 7, 4, 2 }

local cityActionName =
  { "Upgrade actions"
  , "Upgrade library"
  , "Upgrade key"
  , "Upgrade coffers"
  , "Upgrade privilege"
  , "End game VP"
  }


-- Bonus token
local bonusNum = { 2, 3, 2, 4, 2, 2 } -- how many of each type are there

local bonusUpgrade = 1
local bonusSwap = 2
local bonusMove = 3
local bonusExtra = 4
local bonusAct4 = 5
local bonusAct3 = 6

local printedBonus = 7      --printed should be >= this
local bonusPrintedPlace2 = 7
local bonusPrintedMove2 = 8
local bonusPrintedGainPrivilege = 9
local bonusPrintedBuildInGreen = 10
local bonusPrintedReuse2 = 11


function bonusName(m,b)
  if     b == bonusUpgrade then return "Upgrade a skill"
  elseif b == bonusSwap    then return "Swap 2 offices"
  elseif b == bonusMove    then
            return m.modifiedRemove and "Move 3 opponents"
                                     or "Remove 3 workers"
  elseif b == bonusExtra   then return "Build annex"
  elseif b == bonusAct4    then return "Gain 4 actions"
  elseif b == bonusAct3    then return "Gain 3 actions"
  elseif b == bonusPrintedPlace2 then
      return string.format("Place 2 in %s",foreignOptions(m))
  elseif b == bonusPrintedMove2 then return "Move 2 workers, swaps OK"
  elseif b == bonusPrintedGainPrivilege then return "Gain 1 privilege"
  elseif b == bonusPrintedBuildInGreen then return "Office in restricted city"
  elseif b == bonusPrintedReuse2 then
                        return "Reuse 2 workers from a completed route"
  else return string.format("Bonus %d",b)
  end
end

function workerName(t) return (t == trader) and "trader" or "merchant" end

function stopName(t) return (t == stopRoad) and "road" or "ship" end


-- Not a constant, but we want it to go at the top
local GUI
local GLOB_FUN_COUNTER = 0

function DYN_GLOB(f)
  local nm = "HansaTeutonicaDyn_" .. GLOB_FUN_COUNTER
  _G[nm] = f
  GLOB_FUN_COUNTER = GLOB_FUN_COUNTER + 1
  return nm
end

function DEL_DYN(nm)
  _G[nm] = nil
end

--------------------------------------------------------------------------------

local turnSave
local actSaves = {}
local undoing = false
