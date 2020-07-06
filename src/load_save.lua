function onLoad(state)
  if state and state ~= "" then
    local g = JSON.decode(state)
    if not g or g.version ~= version
      then mainMenu()
      else newGUI(g, ||nextTurn(g))
    end
  else
    mainMenu()
  end
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



