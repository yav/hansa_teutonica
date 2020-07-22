function onLoad(s)
  if s ~= nil and g ~= "" then
    g = JSON.decode(s)
    if g ~= nil and g.version == versonMaj then
      resumeGame(g)
    else mainMenu()
    end
  else
    mainMenu()
  end

end

function onSave()
  return saved_game
end

function doSave(g)
  saved_game = JSON.encode(g)
  clearUndo(g)
end

function resumeGame(g)
  newGUI(g,||startTurn(g))
end

function clearUndo(g)
  disableUndo()
  undo_stack = {}
end

function pushUndo(json)
  push(undo_stack,json)
  enableUndo()
end

function undo(obj,c,alt)
  if undoing then return end
  undoing = true
  local json = pop(undo_stack)
  if json == nil then undoing = false; return end
  local arr = JSON.decode(json)
  local g,p = arr[1],arr[2]
  if g.controlledBy[p] == c then
    newGUI(g, function() undoing = false; takeAction(g,p) end)
  else
    push(undo_stack,json)
    say(string.format("%s cannot undo %s's turn"
                     , playerColorBB(c), playerColorBB(p)))
    undoing = false
  end
end



function mainMenu()
  -- XXX
  local ps ={}
  ps["Orange"] = "White"
  ps["Pink"]   = "White"
  resumeGame(newGame(ps))
end
