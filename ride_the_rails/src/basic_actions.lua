
function doAddTrain(g,loc,company,k)
  local map   = g.map
  local route = map.routes[company]
  if route == nil then route = locMapEmpty(); map.routes[company] = route end
  locMapInsert(route,loc,true)
  locMapLookup(map.locations,loc).trains[company] = true

  spawnTrainAt(loc,company,k)
end

