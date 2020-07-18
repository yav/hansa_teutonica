function onLoad()
  local q = actQ()
  local ps = { "Orange", "Brown", "White" }
  local g = newGame(ps)
  q.enQ(||newGUI(g, q.next))
  q.enQ(||test(g))
end

function test(g)
  local q = actQ()
  spawnTemple("Green",gridToWorld(location(10,10),1.5),1,q.next)
  spawnTemple("Green",gridToWorld(location(10,18),1.5),4,q.next)

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
