function onLoad(s)
  addRuleSummary()
  if s ~= nil then
    g = JSON.decode(s)
    if g ~= nil and g.version == versonMaj then
      resumeGame(g)
    else mainMenu()
    end
  else
    mainMenu()
  end

end

function onSave()
  return saved_game
end

function doSave(g)
  saved_game = JSON.encode(g)
  return saved_game
end

function resumeGame(g)
  newGUI(g,||takeTurn(g))
end

function pushUndo(p,json)
  undo_state = json
  enableUndo(p)
end

function undo(obj,c,alt)
  if undoing then return end
  undoing = true
  local json = undo_state
  if json == nil then undoing = false; return end
  local g = JSON.decode(json)
  if g.controlledBy[getCurrentPlayer(g)] == c then
    undo_state = nil
    disableUndo()
    newGUI(g, function() undoing = false; takeTurn(g) end)
  else
    say(string.format("%s cannot undo %s's turn"
                     , playerColorBB(c), playerColorBB(p)))
    undoing = false
  end
end

--------------------------------------------------------------------------------

function onPlayerChangeColor()
  if start_menu == nil then return end
  updateStartMenu()
end

function updateStartMenu()
  local ps = Player.getPlayers()

  local known = {}
  for i,c in ipairs(start_menu.colors) do
    known[c] = true
    local cc = start_menu.players[c]
    if c == cc then
      start_menu.players[c] = nil
    end
  end

  for _,p in ipairs(ps) do
    if known[p.color] then
      start_menu.players[p.color] = p.color
    end
  end

  local function labFor(c)
    local sq = playerColorNote(c,"■")

    local cc = start_menu.players[c]
    if cc == nil then return sq end

    local ccc = start_menu.players[cc]
    if cc ~= ccc then
      start_menu.players[c] = nil
      return sq
    end

    if c == cc then return "Player " .. sq end
    return sq .. " controlled by " .. playerColorNote(cc,"■")
  end

  for i,c in ipairs(start_menu.colors) do
    start_menu.menu.editButton({index=i,label=labFor(c)})
  end
end


function mainMenu()
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
  start_menu = { menu = nil
               , players = {}
               , pnum = 0
               , colors = { "Red", "Yellow", "Green", "Blue" }
               }


  spawnMenu(0,0,function(menu)
    start_menu.menu = menu

    spawnMenuItem(nil,menu,0,
                      string.format("Byzantium %d.%d",versonMaj,versonMin),nil)
    local funs = {}
    for i,c in ipairs(start_menu.colors) do
      funs[i] = DYN_GLOB(function(o,clickBy,a)
        local cc = start_menu.players[c]
        if cc ~= c then
          if cc == clickBy then
            start_menu.players[c] = nil
          else
            start_menu.players[c] = clickBy
          end
          updateStartMenu()
        end
      end)
      spawnMenuItem(nil,menu,i, "", funs[i])
    end
    updateStartMenu()

    push(funs,DYN_GLOB(function()
      local ps = start_menu.players
      local count = 0
      for _ in pairs(ps) do count = count + 1 end
      if count > 4 then
        say("This game only plays up to 4 players.")
        return
      end
      if count < 2 then
        say("This game requires at least 2 players.")
        return
      end

      for _,f in ipairs(funs) do
        DEL_DYN(f)
      end
      start_menu = nil
      resumeGame(newGame(ps))
    end))
    spawnMenuItem(nil,menu,#start_menu.colors+1, "Start", funs[#funs])
  end)

end
