-- GUI
local GUI
local menu_x              = 24
local menu_y              = -3
local turn_order_x        = 22
local turn_order_y        = -1
local player_x            = -25
local player_y            = 24
local supply_x            = 29
local supply_y            = 7
local board_z             = 1
local train_z             = 1.5
local meeple_z            = 1.1
local disc_z              = 1.2
local txt_question_z      = 1.2
local map_question_z      = 1.5
local map_question_hi_z   = 2.5

-- Answer types
local ans_location    = 1
local ans_menu        = 2

-- Directions
local north_west    = 1
local north_east    = 2
local east          = 3
local south_east    = 4
local south_west    = 5
local west          = 6
local allDirections = { north_west, north_east, east
                      , south_east, south_west, west
                      }
local dirDRow       = {  1, 1, 0, -1, -1,  0 }
local dirDCol       = { -1, 0, 1,  1,  0, -1 }

-- Phases
local phaseTakeShare    = 1
local phaseBuildTrack   = 2
local phaseRideTheRails = 3
local pahseName = { "Take a Share", "Build Railroad Tracks", "Ride the Rails" }

-- Companies
local companyName = { "Blue", "Red", "Orange", "Yellow", "Purple", "Black" }
local companyRoundStart = { Blue = 1, Red = 1, Orange = 2
                          , Yellow = 3, Purple = 4, Black = 5 }

function companyColor(name)
  return Color.fromString(name)
end

-- Terrain
local terrainCity       = 1
local terrainPlains     = 2
local terrainMountains  = 3
local terrainSuburb     = 4   -- for multi-hex cities

-- Bonuses
local bonusRecurring    = 1
local bonusOnlyFirst    = 2

-- Used to restrict railorads
local startingCity      = 1 -- at most 2 per railroad
local parisEntry        = 2 -- at most 1 per railroad
local railroadLimit     = { 2, 1 }


