
function onLoad()
  local controlledBy = {}
  controlledBy["Red"]   = "White"
  controlledBy["Blue"]  = "White"
  controlledBy["Green"] = "White"
  local g = newGame(controlledBy)
  newGUI(g,||test(g))
end


function test(g)
  local q = actQ()
  q.enQ(||doPlaceArmy(g,"Red",byzantium,"Damascus",q.next))
  q.enQ(||doPlaceArmy(g,"Green",byzantium,"Palmyra",q.next))
  q.enQ(function()
    askText(g,"Green","Where?", { {text="Mosul",val="Mosul"}
                                , {text="Antioch",val="Antioch"}
                                }
            , |x| doMoveArmy(g,"Green",byzantium,x))
  end)
end


