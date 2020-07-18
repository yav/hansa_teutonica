function onLoad()
  local q = actQ()
  newGUI(q.next)
  q.enQ(test)
end

function test()
  -- askText("White","What next?",{{text = "one", val = 1}},say)
  local map = newMap()
  local opts = {}
  for loc,t in locsIn(map) do
    if t.terrain == land then
      push(opts,loc)
    end
  end
  askMapLoc("Yellow","Board question",opts,
  function(loc)
    say(string.format("r=%d,c=%d",loc.row,loc.col))
  end)

end
