
function mainMenu()
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end

  local ok = {}
  for _,c in ipairs({"Red","Yellow","Green","Blue","Purple"}) do
    ok[c] = true
  end

  local funs = {}
  local function startGame(map3,map45)
    local ps      = Player.getPlayers()
    local players = {}
    for i,p in ipairs(ps) do
      local c = p.color
      if not ok[c] then
        say(c .. " is not a supported color, please change it.")
        return
      end
      players[i] = { color = c, controlledBy = c }
    end

    local function doStart()
      for _,f in ipairs(funs) do
        DEL_DYN(f)
      end
      local mp = (#players < 4) and map3 or map45
      local g  = newGame(players,mp)
      newGUI(g, ||nextTurn(g))
    end

    if #players < 3 then
      chooseDummy(players,doStart)
    else doStart()
    end
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
    local msg = string.format("Hansa Teutonica (version %d)",version)
    spawnMenuItem(nil,m,-2,msg,nil)
    spawnMenuItem(nil,m,0,"Choose map:",nil)

    for i,o in ipairs(opts) do
      local fun = DYN_GLOB(o.val)
      push(funs,fun)
      spawnMenuItem(nil,m,i,o.text,fun)
    end
  end)
end


function chooseDummy(ps,k)
  local dummy = {}
  for _,c in ipairs({"Red","Yellow","Green","Blue","Purple"}) do
    dummy[c] = true
  end

  local dummyPlayers = {}
  local dummyNum = 0

  local function heading()
    local h = "Players"
    for i,pi in ipairs(ps) do
      h = h .. playerColorNote(pi.color, " ■")
      dummy[pi.color] = nil
    end
    for d,c in pairs(dummyPlayers) do
      h = h .. playerColorNote(d, " ■") ..
               playerColorNote(c, "*")
    end
    return h
  end


  spawnMenu(10,0,function(m)
    local h = "Players"
    for i,pi in ipairs(ps) do
      h = h .. playerColorNote(pi.color, " ■")
      dummy[pi.color] = nil
    end
    spawnMenuItem(nil,m,0,heading(),nil)
    spawnMenuItem(nil,m,-1,"Need at least 3",nil)

    local ix = 1
    local funs = {}
    for d,_ in pairs(dummy) do
      local lab  = playerColorNote(d, " ■")
      local fun = DYN_GLOB(function(obj,c,alt)
        local c1 = dummyPlayers[d]
        if c == c1 then
          dummyPlayers[d] = nil
          dummyNum = dummyNum - 1
        else
          dummyPlayers[d] = c
          dummyNum = dummyNum + 1
        end
        m.editButton({index = 0, label = heading{}})
      end)
      spawnMenuItem(nil,m,ix,lab,fun)
      funs[ix] = fun
      ix = ix + 1
    end

    funs[ix] = DYN_GLOB(function()
      if dummyNum + #ps < 3 then
        say("Need at least 3 players.")
        return
      end
      for d,c in pairs(dummyPlayers) do
        push(ps, { color = d, controlledBy = c })
      end
      for _,f in ipairs(funs) do
        DEL_DYN(f)
      end
      k()
    end)

    spawnMenuItem(nil,m,ix,"Start",funs[ix])

  end)
end
