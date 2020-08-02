function onLoad(state)
  local ctrl = {}
  ctrl["White"] = "White"
  local map = mapUSA()
  log(map)

  local g = newGame(ctrl,map)
  newGUI(g)
end



