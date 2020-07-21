function onLoad()
  local q = actQ()
  local ps = { "Orange", "Pink" }
  local g = newGame(ps)
  newGUI(g,||startTurn(g))
end
