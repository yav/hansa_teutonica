function onLoad(state)
  local ctrl = {}
  ctrl["White"] = "White"
  local map = mapUSA()

  local g = newGame(ctrl,map)
  newGUI(g,||test(g,"Red"))
end


function test(g,co)
  local txtOpts = { { text = "Blue",   val = "Blue" }
                  , { text = "Red",    val = "Red" }
                  , { text = "Yellow", val = "Yellow" }
                  }
  local mapOpts = buildLocations(g.map,co,true)

  askMapLoc(g,"White","Which one",mapOpts,txtOpts,function(ans)
    if ans.tag == ans_location then
      doAddTrain(g,ans.location,co,||test(g,co))
    else
      test(g,ans.value)
    end
  end)
end



