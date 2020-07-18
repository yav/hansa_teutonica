
function terLand()
  return { terrain = land, entity = nil }
end

function terCanal()
  return { terrain = canal, entity = nil, bridge = nil }
end
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Entities

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
-- Players

function newPlayer()
  local p = { VP = 0, AP = 0, savedAP = 0, leader = nil, temples = {} }
  p.temples[1] = 3
  p.temples[2] = 3
  p.temples[3] = 2
  p.temples[4] = 1
  return p
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
    if not r then return nil end
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


function findRegion(map,start)
  local startTerrain = locMapLookup(map,start).terrain
  local visited  = locMapEmpty()
  local todo     = { start }
  local lastTodo = #todo
  local nextTodo = 1

  while nextTodo <= lastTodo do
    local loc = todo[nextTodo]
    todo[nextTodo] = nil
    nextTodo = nextTodo + 1

    if not locMapLookup(visited,loc) then
      local spot = locMapLookup(map,loc)
      if spot and spot.terrain == startTerrain then
        locMapInsert(visited,loc,true)
        if spot.terrain ~= canal or
           spot.bridge  == nil   or
           locationSame(loc,start) then
             for _,dir in ipairs({north,east,south,west}) do
               lastTodo = lastTodo + 1
               todo[lastTodo] = neighbour(loc,dir)
             end
        end
      end
    end
  end

  if startTerrain == canal then
    local bridges = locMapEmpty()
    for l,_ in locsIn(visited) do
      if not locationSame(l,start) and locMapLookup(map,l).bridge then
        locMapInsert(bridges,l)
      end
    end
    return bridges
  end

  return visited
end


