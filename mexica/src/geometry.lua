
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


-- `palaceOK` says if we should count spots with palace on them.
-- Those are ok for movement but not for canal placement.
function freeLandSpot(spot,palaceOK)
  return
    spot.terrain == land and
    spot.leader == nil and
    (spot.entity == nil or palaceOK and spot.entity.entity == palace)
end

function freeLandNeigbours(map,loc,palaceOK)
  local result = locMapEmpty()
  for _,dir in ipairs(allDirs) do
    local candidate = neighbour(loc,dir)
    local spot = locMapLookup(map,candidate)
    if spot and freeLandSpot(spot,palaceOK) then
      locMapInsert(result,candidate,true)
    end
  end
  return result
end

-- `double` says that we want only spots that will accomodate a double piece
function freeCanalSpots(map,double)
  local result = locMapEmpty()
  for loc,spot in locsIn(map) do
    if freeLandSpot(spot,false) and
      not (double and locMapIsEmpty(freeLandNeigbours(map,loc,false)))
    then
      locMapInsert(result,loc,true)
    end
  end
  return result
end

function canMoveOnFoot(map,to,dir)
  local toSpot = locMapLookup(map,to)
  if toSpot.leader ~= nil then return false
  if toSpot.terrain == land then return toSpot.entity == nil or
                                        toSpot.entity.entity == palace end
  if toSpot.entity and toSpot.entity.entity == bridge then
    if toSpot.entity.direction == east_west then
      return dir == east or dir == west
    else
      return dir == north or dir == south
    end
  end
  return false
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
        if d < limit and canMoveOnFoot(map,to,dir) then
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



