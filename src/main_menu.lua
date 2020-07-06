function mainMenu()
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end

  local players = Player.getPlayers()
  local pnum    = #players

  -- These are for testing
  players = {"Purple","Yellow"}
  pnum    = 3 -- used to select map
  -- end testing

  local funs = {}
  local function startGame(map3,map45)
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
    local msg = string.format("Starting a %d player game",pnum)
    spawnMenuItem(nil,m,-4,msg,nil)

    msg = ""
    for _,c in ipairs(players) do
      if msg == "" then msg = playerColorNote(c,"■")
                   else msg = msg .. ' ' .. playerColorNote(c,"■")
      end
    end
    spawnMenuItem(nil,m,-3,"Players " .. msg,nil)
    spawnMenuItem(nil,m,0,"Choose map:",nil)

    for i,o in ipairs(opts) do
      local fun = DYN_GLOB(o.val)
      push(funs,fun)
      spawnMenuItem(nil,m,i,o.text,fun)
    end
  end)
end
