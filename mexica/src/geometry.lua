
function gridToWorld(loc,z)
  local x = (loc.col - 14) * 2 - 1
  local y = (loc.row - 9) * -2 + 1
  return Vector(x,z,y)
end


-- Find a region of connected land or canal.
-- In the case of canals we stop at bridges
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
           not spot.entity or
           spot.entity.entity ~= bridge or
           locationSame(loc,start) then
             for _,dir in ipairs(allDirs) do
               lastTodo = lastTodo + 1
               todo[lastTodo] = neighbour(loc,dir)
             end
        end
      end
    end
  end

  return visited
end


function freeCanalSpot(spot)
  return spot.terrain == land and
         spot.leader == nil and
         spot.entity == nil and
         not spot.inDistrict and
         spot.bridgeFoundation == 0
end

function freeCanalNeihbours(map,loc)
  local result = locMapEmpty()
  for _,dir in ipairs(allDirs) do
    local candidate = neighbour(loc,dir)
    local spot = locMapLookup(map,candidate)
    if spot and freeCanalSpot(spot) then
      locMapInsert(result,candidate,true)
    end
  end
  return result
end

-- `double` says that we want only spots that will accomodate a double piece
function freeCanalSpots(map,double)
  local result = locMapEmpty()
  for loc,spot in locsIn(map) do
    if freeCanalSpot(spot) and
      not (double and locMapIsEmpty(freeCanalNeihbours(map,loc)))
    then
      locMapInsert(result,loc,true)
    end
  end
  return result
end

function canMoveOnFoot(map,from,to,dir)
  local toSpot = locMapLookup(map,to)
  if not canStepOn(toSpot) then return false end

  local function checkBridge(spot)
    -- not a bridge
    if spot.entity == nil or spot.entity.entity ~= bridge then return true

    -- a bridge
    if spot.entity.direction == east_west then
       return dir == east or dir == west
    else
       return dir == north or dir == south
    end
  end

  -- we can only enter/leave a bridge in 2 directions
  return checkBridge(locMapLookup(map,from)) and checkBridge(toSpot)
end

function canStepOn(spot)
  if spot.leader ~= nil then return false
  if spot.terrain == land then
    return spot.entity == nil or spot.entity.entity == palace
  end

  return spot.entity ~= nil and spot.entity.entity == bridge
end

function moveOnFoot(map,start,limit)
  local visited = locMapEmpty()
  local todo = { { here = start, dist = 0 } }
  local nextTodo = 1
  local nextAdd = 2

  while nextTodo < nextAdd do

    local this = todo[nextTodo]
    nextTodo = nextTodo + 1
    local loc = this.here
    local d   = this.dist

    local known = locMapLookup(visited,loc)
    if known == nil then
      locMapInsert(visited,loc,d)
      for _,dir in ipairs(allDirs) do
        local to = neighbour(loc,dir)
        if d < limit and canMoveOnFoot(map,loc,to,dir) then
          todo[nextAdd] = { here = to, dist = d + 1 }
          nextAdd = nextAdd + 1
        end
      end
    end
  end

  locMapDelete(visited,start)
  return visited

end


function bridgeSpots(map)
  local bridges = locMapEmpty()

  for l,spot in locsIn(map) do
    if spot.terrain == canal and spot.entity == nil then
      local function check(dir)
        local other = locMapLookup(map,neighbour(l,dir))
        return other and other.terrain == land
      end
      local opts = {}
      if check(north) and check(south) then
        push(opts, {val=north_south,text="Vertical"})
      end
      if check(east) and check(west) then
        push(opts, {val=east_west,text="Horizontal"})
      end
      if #opts > 0 then locMapInsert(bridges,l,opts) end
    end
  end

  return bridges
end



function boatConnection(map,start)
  local result = locMapEmpty()
  for l,_ in locsIn(findRegion(map,start)) do
    if not locationSame(l,start) then
      local spot = locMapLookup(map,l)
      if spot.entity and spot.entity.entity == bridge then
        locMapInsert(result,l,true)
      end
    end
  end
  return result
end


function moveByBoat(map,start,limit)
  local result = locMapEmpty()
  local visited = locMapEmpty()
  local todo = { { here = start, dist = 0 } }
  local nextTodo = 1
  local nextAdd = 2

  while nextTodo < nextAdd do

    local this = todo[nextTodo]
    nextTodo = nextTodo + 1
    local loc = this.here
    local d   = this.dist

    if locMapLookup(visited,loc) == nil then
      locMapInsert(visited,loc,true)
      if not locationSame(loc,start) and
        locMapLookup(map,loc).leader == nil then
        locMapInsert(result,loc,d)
      end

      if d < limit then
        for to,_ in locsIn(boatConnection(map,loc)) do
          todo[nextAdd] = { here = to, dist = d + 1 }
          nextAdd = nextAdd + 1
        end
      end
    end
  end

  return result
end

function teleportSpots(map)
  local result = locMapEmpty()
  for l,spot in locsIn(map) do
    if canStepOn(spot) then locMapInsert(result,l,true) end
  end
  return result
end

