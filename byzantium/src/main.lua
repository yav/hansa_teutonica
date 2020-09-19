
function onLoad()
  local g = newGame({"Red","Green","Blue","Yellow"})
  newGUI(g,||test(g))
end


function test(g)
  g.map.cities["Tabuk"].controlledBy = "Red"
  g.map.cities["Tabuk"].fortified = true
  changeAvailableWorkers(g,"Blue",-5)
  changeEliteArmy(g,"Red",arabs,17)
  changeRoyalty(g,"Red",byzantium,true)
  changeRoyalty(g,"Red",arabs,true)
  redrawCity(g,"Tabuk",||1)
end


