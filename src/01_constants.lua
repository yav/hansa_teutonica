-- Board offset
local boardDX = -14
local boardDY = 0
local boardPieceZ = 1.5

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

local cityActionName =
  { "Upgrade actions"
  , "Upgrade movement (book)"
  , "Upgrade key"
  , "Upgrade hiring (bag)"
  , "Upgrade office types"
  , "End game VP"
  }


-- Bonus token
local bonusUpgrade = 1
local bonusSwap = 2
local bonusMove = 3
local bonusExtra = 4
local bonusAct4 = 5
local bonusAct3 = 6

local printedBonus = 7      --printed should be >= this
local bonusPrintedPlace2 = 7
local bonusPrintedMove2 = 8


local bonusName =
  { "Bonus: Upgrade a skill"
  , "Bonus: Swap 2 offices"
  , "Bonus: Move others 3 times"
  , "Bonus: Build external office"
  , "Bonus: Gain 4 actions"
  , "Bonus: Gain 3 actions"
  , "Bonus: Place 2 in Scotland/England"
  , "Bonus: Move any 2 workers, swaps OK"
  }


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
