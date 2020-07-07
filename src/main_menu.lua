function mainMenu()
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end

  local funs = {}
  local function startGame(map3,map45)
    local ps      = Player.getPlayers()
    local players = {}
    for i,p in ipairs(ps) do
      players[i] = p.color
    end
    -- ps = { "Purple", "Blue", "Green" } -- XXX: testing
    local pnum = #ps

    if pnum < 3 then
      broadcastToAll("Need at least 3 players.")
      return
    end

    for _,f in ipairs(funs) do
      DEL_DYN(f)
    end

    local mp = (pnum < 4) and map3 or map45
    local g  = newGame(players,mp)
    newGUI(g, ||nextTurn(g))
  end

  local opts =
    { { text  = "Hansa Teutonica"
      , val   = ||startGame(originalMap23,originalMap)
      }
    , { text  = "East Expansion"
      , val   = ||startGame(eastMap,eastMap)
      }
    , { text  = "Britannia"
      , val   = ||startGame(britaniaMap23,britaniaMap)
      }
    }

  spawnMenu(0,0,function(m)
    spawnMenuItem(nil,m,0,"Choose map:",nil)

    for i,o in ipairs(opts) do
      local fun = DYN_GLOB(o.val)
      push(funs,fun)
      spawnMenuItem(nil,m,i,o.text,fun)
    end
  end)
end
