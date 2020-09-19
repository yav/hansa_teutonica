
function onLoad()
  local g = newGame({"Red","Green","Blue","Yellow"})
  newGUI(g,||test(g))
end


function test(g)
  local q = actQ()
  q.enQ(||doPlaceArmy(g,"Red",byzantium,"Damascus",q.next))
  q.enQ(||doPlaceArmy(g,"Green",byzantium,"Palmyra",q.next))
  q.enQ(function()
    doMoveArmy(g,"Red",byzantium,"Palmyra")
    doMoveArmy(g,"Green",byzantium,"Damascus")
  end)
end


