function onLoad(state)
  local g
  if state and state ~= "" then
    g = JSON.decode(state)
  else
    g = newGame({"Green","Purple" }, britaniaMap)
  end
  newGUI(g, ||nextTurn(g))
end

function onSave()
  if turnSave then return turnSave else return nil end
end



