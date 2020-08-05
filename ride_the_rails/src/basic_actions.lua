function doGainMoney(g,p,n)
  if n == 0 then return end
  local s = g.playerState[p]
  s.money = s.money + n
  editMoney(p,s.money)
end


function doAddTrain(g,loc,company,k)
  local map   = g.map
  local route = map.routes[company]
  local starting = false
  if route == nil then
    route = locMapEmpty();
    map.routes[company] = route
    starting = true
  end
  local spot = locMapLookup(map.locations,loc)

  if not starting and spot.bonus > 0 then
    doGainMoney(g,g.players[g.currentPlayer],spot.bonus)
    if spot.bonusType == bonusOnlyFirst then
      spot.bonus = 0
      spot.bonusType = nil
    end
  end

  locMapInsert(route,loc,true)
  spot.trains[company] = true
  local n = g.supply[company]
  g.supply[company] = n - 1

  spawnTrainAt(loc,company,k)
  editTrainSupply(company,g.supply[company])
end


function doInvest(g,company,k)
  local p           = g.players[g.currentPlayer]
  local s           = g.playerState[p]
  local shares      = s.shares
  g.supply[company] = g.supply[company] - 1
  shares[company]   = shares[company] + 1

  editTrainSupply(company,g.supply[company])
  spawnShare(s,company,shares[company],k)
end


function doPickTraveller(g,loc)
  locMapLookup(g.locations, loc).passenger = false
  g.traveller         = loc
  locMapInsert(g.travellerVisited,loc,true)
  doGainMoney(g,g.players[g.currentPlayer],1)

  local gui_spot = locMapLookup(GUI.map, loc)
  local p = gui_spot.passenger
  gui_spot.passenger = nil
  GUI.traveller = p
  local pos = p.getPosition()
  pos.y = traveller_z
  p.setPosition(pos)

end

function doMoveTraveller(g,loc,company)
  g.traveller = loc
  locMapInsert(g.travellerVisited,loc,true)
  doGainMoney(g,g.players[g.currentPlayer],1)
  for p,s in pairs(g.playerState) do
    doGainMoney(g,p,s.shares[company])
  end

  local pos = gridToWorld(loc,traveller_z)
  GUI.traveller.setPositionSmooth(pos,false,false)
end


