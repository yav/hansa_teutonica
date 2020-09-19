
function onLoad()
  local g = newGame({"Red","Green","Blue","Yellow"})
  newGUI(g,||test(g))
end


function test(g)
  g.map.cities["Tabuk"].controlledBy = "Red"
  g.map.cities["Tabuk"].fortified = true
  redrawCity(g,"Tabuk",||1)
end


