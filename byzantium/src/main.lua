
function onLoad()
  local controlledBy = {}
  controlledBy["Red"]   = "White"
  controlledBy["Blue"]  = "White"
  -- controlledBy["Green"] = "White"
  -- controlledBy["Yellow"] = "White"
  local g = newGame(controlledBy)
  newGUI(g,||test(g))
end


function test(g)
  takeTurn(g)
end


