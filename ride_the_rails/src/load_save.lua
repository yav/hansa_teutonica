function onLoad(state)
  local ctrl = {}
  ctrl["White"] = "White"
  local map = testMap()

  local g = newGame(ctrl,map)
  newGUI(g)
end



