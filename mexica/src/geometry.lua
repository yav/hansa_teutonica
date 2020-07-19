
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



