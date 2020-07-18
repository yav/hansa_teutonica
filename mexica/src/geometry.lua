
function gridToWorld(loc,z)
  local x = (loc.col - 14) * 2 - 1
  local y = (loc.row - 9) * -2 + 1
  return Vector(x,z,y)
end


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

  return visited
end


