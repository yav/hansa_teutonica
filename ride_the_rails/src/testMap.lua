
function testMap()
  local map = newMap()
  for r = -3,3 do
    for c = -4,4 do
      addSpot(map, location(r,c), newSpot(terrainPlains))
    end
  end
  addSpot(map, location(0,0), newSpot(terrainCity))
  addSpot(map, location(0,1), newSpot(terrainSuburb))
  addWall(map,location(0,0),north_east)
  addWall(map,location(0,1),north_west)
  return map
end

