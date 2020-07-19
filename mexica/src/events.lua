function onLoad()
  local q = actQ()
  local ps = { "Orange", "Brown", "White" }
  local g = newGame(ps)

  local spot = locMapLookup(g.map, location(9,16))
  spot.entity = entBridge(east_west)
  spot.leader = "Green"


  local spot = locMapLookup(g.map, location(9,15))
  spot.entity = entDistrict(7)

  q.enQ(||newGUI(g, q.next))
  q.enQ(||test(g))
end

function test(g)
  startTurn(g)

end
