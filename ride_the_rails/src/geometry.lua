-- Location of neighbour in given direction
function neighbourLoc(loc,dir)
  return location(loc.row + dirDRow[dir], loc.col + dirDCol[dir])
end

-- Locations of neighbours in all directions,
-- as long as not separated by a wall.
-- Note that we don't check that the neigbour exists, simply that if
-- there was something, it is not blocked by a wall.
function directNeighbours(map,l)
  local result = {}
  for _,d in ipairs(allDirections) do
    local other   = neighbourLoc(l,d)
    local walls   = locMapLookup(map.disconnected,l)
    if walls == nil or locMapLookup(walls,other) ~= true then
      push(result,other)
    end
  end
  return result
end


-- Compute the locations surrounding a set of hexes.
function regionNeighbours(map,reg)
  local result = locMapEmpty()
  for l,_ in locsIn(reg) do
    for _,n in ipairs(directNeighbours(map,l)) do
      if locMapLookup(reg,n) ~= true then
        locMapInsert(result,n,true)
      end
    end
  end
  return result
end


-- Returns the set of locations of cities reachable using one link of
-- the given company
function getConnectedCitiesForOne(map,start,compnay)
  local result  = locMapEmpty()
  local visited = locMapEmpty()
  local todo    = { start }
  local front   = 1
  local back    = 2

  while front < back do
    local loc = todo[front]
    front = front + 1
    if locMapLookup(visited,loc) ~= true then
      locMapInsert(visited,loc,true)
      local spot = locMapLookup(map.locations,loc)
      if spot ~= nil and spot.trains[compnay] == true then
        if spot.terrain == terrainCity and not sameLocation(loc,start) then
          locMapInsert(result,loc,true)
        else
          for _,newLoc in ipairs(directNeighbours(map,start)) do
            todo[back] = newLoc
            back = back + 1
          end
        end
      end
    end
  end

  return result
end

-- Returns a location map, mapping the locations of reachabe cities
-- to the set of companies one can use to get to there.  The set in the
-- map is never empty.
function getConnectedCities(map,start)
  local result = locMapEmpty()
  local spot = locMapLookup(map.locations,start)
  if spot == nil then return result end

  for company,_ in pairs(spot.trains) do
    for loc,_ in locsIn(getConnectedCitiesForOne(map,start,company)) do
      local opts = locMapLookup(loc)
      if opts == nil then opts = {}; locMapInsert(loc,opts); end
      opts[company] = true
    end
  end

  return result
end

