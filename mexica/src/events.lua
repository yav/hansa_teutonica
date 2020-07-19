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
  counterChange(GUI.canal1, 3)

  local q = actQ()
  -- askText("White","What next?",{{text = "one", val = 1}},say)
  -- local opts = {}
  -- for loc,t in locsIn(map) do
  --   if t.terrain == land then
  --     push(opts,loc)
  --   end
  -- end
  -- askMapLoc("Yellow","Board question",opts,
  -- function(loc)
  --   say(string.format("r=%d,c=%d",loc.row,loc.col))
  -- end)

end
