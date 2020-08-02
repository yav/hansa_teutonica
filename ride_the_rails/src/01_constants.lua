-- GUI
local GUI
local board_z  = 1
local train_z  = 2
local meeple_z = 1.2
local disc_z   = 1.2

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

function companyColor(name)
  return Color.fromString(name)
end

-- Terrain
local terrainCity       = 1
local terrainPlains     = 2
local terrainMountains  = 3
local terrainSuburb     = 4   -- for multi-hex cities

-- Bonuses
local bonusEveryBuilt   = 1
local bonusOnlyFirst    = 2

-- Used to restrict railorads
local startingCity      = 1 -- at most 2 per railroad
local parisEntry        = 2 -- at most 1 per railroad

