

-- Terrain types
local land  = 1
local canal = 2

function terLand()
  return { terrain = land, entity = nil }
end

function terCanal()
  return { terrain = canal, entity = nil, bridge = nil }
end
--------------------------------------------------------------------------------



-- Directions
local north = 1
local east  = 2
local south = 3
local west  = 4

local dirDX = { 0, 1, 0, -1 }
local dirDY = { -1, 0, 1, 0 }

-- Bridge directions
local north_south = 1
local east_est    = 2


--------------------------------------------------------------------------------
-- Entities

local leader    = 1
local temple    = 2
local district  = 3
local palace    = 4

function entLeader(owner)
  return { entity = leader, owner = owner }
end

function entTemple(owner,level)
  return { entity = temple, owner = owner, level = level }
end

function entDistrict(size)
  return { entity = district, size = size }
end

function entPalace()
  return { entity = palace }
end

--------------------------------------------------------------------------------





function emptyMap()
  return {}
end

function addCanal(map,loc)
  locMapInsert(map,loc,terCanal())
end

function addEntity(map,loc,entity)
  local spot  = locMapLookup(map,loc)
  spot.entity = entity
end




--------------------------------------------------------------------------------
-- Locations

function location(row,col)
  return { row = row, col = col }
end

function locationSame(x,y)
  return x.row == y.row and x.col == y.col
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
  row[loc.col] = val
end

function locMapLookup(locMap,loc)
  local row = locMap[loc.row]
  if not row then return nil end
  return row[loc.col]
end

function locMapIter(locMap,loc)
  local r
  local row
  if not loc then
    r,row = next(locMap,nil)
  else
    r   = loc.row
    row = locMap[r]
  end

  local c = loc and loc.col or nil

  while true do
    local val
    c,val = next(row,c)
    if c then return location(r,c),val end
    r,row = next(locMap,r)
    if not r then return nil end
    c     = nil
  end
end

function locsIn(locMap)
  return locMapIter,locMap,nil
end
--------------------------------------------------------------------------------


function findRegions(map)
  local repFor = locMapEmpty()

  local function find(l)
    local rep = locMapLookup(repFor,l)
    if not rep then
      return l
    else
      local r1 = find(rep)
      locMapInsert(repFor,l,r1)
      return r1
    end
  end

  local function union(l1,l2)
    local r1 = find(l1)
    local r2 = find(l2)
    if locationSame(r1,r2) then
      return
    else
      locMapInsert(repFor,r1,r2)
    end
  end

  for loc,spot in locsIn(map) do
    if spot.terrain == land then
      local function check(dir)
        local other_loc  = neighbour(loc,dir)
        local other_spot = locMapLookup(map,other_loc)
        if other_spot ~= nil and other_spot.terrain == land then
          union(loc,other_loc)
        end
      end
      check(north)
      check(east)
    end
  end

  local regions = locMapEmpty()
  for loc,spot in locsIn(map) do
    if spot.terrain == land then
      local rep = find(loc)
      if not rep then
        locMapInsert(regions,loc,{loc})
      else
        local members = locMapLookup(regions,rep)
        if not members then members = {}; locMapInsert(regions,rep,members) end
        members[#members + 1] = loc
      end
    end
  end

  return regions
end


