-- Terrain types
local land  = 1
local river = 2

-- Directions
local north = 1
local east  = 2
local south = 3
local west  = 4

local dirDX = { 0, 1, 0, -1 }
local dirDY = { -1, 0, 1, 0 }


-- Entities
local leader    = 1
local bridge    = 2
local temple    = 3
local establish = 4
local palace    = 5   -- XXX

function emptyMap()
  return {}
end

function blankSpot(terrain)
  return
    { terrain = terrain
    , content = {}
    }
end

function addRiver(map,loc)
  local spot = locMapLookup(map,loc)
  if not spot then
    locMapInsert(map,loc,blankSpot(river))
    return
  end
  spot.terrain = river
end


--------------------------------------------------------------------------------
-- Locations

function location(row,col)
  return { row = row, col = col }
end

function neighbour(loc,dir)
  return location(loc.row + dirDY[dir], loc.col + dirDX[dir])
end


--------------------------------------------------------------------------------
-- A map from locations to values

function locMapEmpty()
  return {}
end

function locMapInsert(locMap,loc,val)
  local row = locMap[loc.row]
  if not row then row = {}; locMap[loc.row] = row end
  locMap[loc.col] = val
end

function locMapLookup(locMap,loc)
  local row = locMap[loc.row]
  if not row then return nil end
  return row[loc.col]
end

function locMapIter(locMap)
end
--------------------------------------------------------------------------------


function findRegions(map)
  local repFor = locMapEmpty()

end


