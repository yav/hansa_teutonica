
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
--[[
  q.enQ(function()
    askAction(g,"Green", { actions = { { act = bulgars_1, val = 1 }
                                     , { act = bulgars_2, val = 2 }
                                     }
                         , text = { { text = "Option 1", val = 3 }
                                  , { text = "Option 2", val = 4 }
                                  }

                         }, function(x) log(x); q.next() end)
  end)
--]]
  q.enQ(function()
    askCity(g,"Blue","Pick City",{ "Constantinople", "Tabuk", "Hira" },log)
  end)
end


