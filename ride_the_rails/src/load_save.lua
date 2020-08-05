function onLoad(state)
  local ctrl = {}
  ctrl["Purple"]  = "White"
  ctrl["Pink"]    = "White"
  ctrl["Brown"]   = "White"
  ctrl["White"]   = "White"
  ctrl["Green"]   = "White"
  local map = mapUSA()

  local g = newGame(ctrl,map)
  newGUI(g,||testInvest(g))
end

function testInvest(g)
  local txtOpts = { { text = "Blue",   val = "Blue" }
                  , { text = "Red",    val = "Red" }
                  , { text = "Yellow", val = "Yellow" }
                  }
  local mapOpts = {}

  local p = "Pink"
  askMapLoc(g,p,"Which one",mapOpts,txtOpts,function(ans)
    doInvest(g,p,ans.value,||testInvest(g))
  end)
end



function testPlacingTrains(g,co)
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



