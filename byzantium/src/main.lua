
function onLoad()
  local controlledBy = {}
  controlledBy["Red"]   = "White"
  controlledBy["Blue"]  = "White"
  controlledBy["Green"] = "White"
  local g = newGame(controlledBy)
  newGUI(g,||test(g))
end


function test(g)
  -- takeTurn(g)
  g.actionSpaces[byz_fleet] = "Red"
  chooseRetreat(g,"Green",arabs,"Constantinople",log)
end


