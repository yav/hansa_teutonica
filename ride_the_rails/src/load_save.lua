function onLoad(state)
  local ctrl = {}
  ctrl["White"] = "White"
  local map = mapUSA()

  local g = newGame(ctrl,map)
  newGUI(g)
end



