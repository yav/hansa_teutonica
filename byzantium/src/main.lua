
function onLoad()
  local controlledBy = {}
  controlledBy["Red"]   = "White"
  controlledBy["Blue"]  = "White"
  controlledBy["Green"] = "White"
  local g = newGame(controlledBy)
  newGUI(g,||test(g))
end


function test(g)
  local opts = workerOptions(g,"Green",byzantium,||test(g))
  askText(g,"Green", "Pay how?", opts, |f|f())

end


