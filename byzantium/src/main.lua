
function onLoad()
  local controlledBy = {}
  controlledBy["Red"]   = "White"
  controlledBy["Blue"]  = "White"
  controlledBy["Green"] = "White"
  local g = newGame(controlledBy)
  newGUI(g,||test(g))
end


function test(g)
  askText(g, "Green", "Worker?", workerOptions(g,"Green",byzantium),
    function(f)
       f()
       test(g)
    end)

end


