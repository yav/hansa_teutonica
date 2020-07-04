function onLoad(state)
  local g
  if state and state ~= "" then
    g = JSON.decode(state)
  else
    g = newGame({"Green","Purple" }, britaniaMap)
  end
  -- newGUI(g, ||nextTurn(g))
  newGUI(g, ||test(g))
end

function onSave()
  if turnSave then return turnSave else return nil end
end

function checkPoint(g)
  push(actSaves, JSON.encode(g))
  GUI.undo.setInvisibleTo({})
end

function maybeHideUndo(g)
  if (#actSaves == 0) then
    GUI.undo.setInvisibleTo(g.players)
  end
end

function undoAction(o,p,alt)
  if undoing then return end
  undoing = true
  local n = #actSaves
  if n == 0 then undoing = false; return end
  local g = JSON.decode(actSaves[n])
  if mayPress(g.players[g.curPlayer],p) then
    actSaves[n] = nil
    newGUI(g,function() undoing = false; takeActions(g) end)
  end
end



