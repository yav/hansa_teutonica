
function reset()
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
end

function onLoad(s)

  reset()

  GUI = newGUI()

  q = actQ()
  q.enQ(|| spawnBasics(q.next))
  q.enQ(|| staticMapSetup(q.next))
  q.enQ(function ()
    local gs = loadGame(s)
    if not gs then noGame()
              else finishGUI(gs,||takeTurn(gs)) end
  end)

end




local saveState1 = nil -- one older
local saveState = nil
local cleanSave

function onSave()
  return saveState
end

function saveGame(gs)
  saveState1 = saveState
  saveState = JSON.encode(gs)
  cleanSave = true
end

function loadGame(s)
  if not s then return nil end
  local gs = JSON.decode(s)
  if not gs or gs.version ~= VERSION then return nil end

  saveState1 = nil
  saveState = s
  cleanSave = true
  return gs
end


function undoTurn()
  onLoad(saveState)
end


