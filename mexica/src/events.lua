function onLoad()
  local q = actQ()
  local ps = { "Orange", "Pink", "Blue", "Green" }
  local g = newGame(ps)

  q.enQ(||newGUI(g, q.next))
  q.enQ(||doPlaceCanal(g,location(6,16),q.next))
  q.enQ(||doPlaceCanal(g,location(10,4),q.next))
  q.enQ(||doBuildBridge(g,location(10,4),north_south,q.next))
  q.enQ(||doBuildBridge(g,location(10,16),east_west,q.next))
  q.enQ(||doBuildTemple(g,"Blue",location(11,12),3,q.next))
  q.enQ(||doPlaceLeader(g,location(10,10),"Orange",q.next))
  q.enQ(||doPlaceLeader(g,location(5,5),"Pink",q.next))
  q.enQ(||doPlaceLeader(g,location(10,15),"Blue",q.next))
  q.enQ(||doPlaceLeader(g,location(10,16),"Green",q.next))
  q.enQ(||test(g))
end

function test(g)
  startTurn(g)
end
