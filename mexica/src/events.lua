function onLoad(s)
  if s ~= nil and g ~= "" then
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
  clearUndo(g)
end

function resumeGame(g)
  Grid.type = 1
  Grid.opacity = 0.4
  Grid.snapping = 3
  Grid.show_lines = true
  newGUI(g,||startTurn(g))
end

function clearUndo(g)
  disableUndo()
  undo_stack = {}
end

function pushUndo(json)
  push(undo_stack,json)
  enableUndo()
end

function undo(obj,c,alt)
  if undoing then return end
  undoing = true
  local json = pop(undo_stack)
  if json == nil then undoing = false; return end
  local arr = JSON.decode(json)
  local g,p = arr[1],arr[2]
  if g.controlledBy[p] == c then
    newGUI(g, function() undoing = false; takeAction(g,p) end)
  else
    push(undo_stack,json)
    say(string.format("%s cannot undo %s's turn"
                     , playerColorBB(c), playerColorBB(p)))
    undoing = false
  end
end

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
    return sq .. playerColorNote(cc,"*")
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
               , colors =
                  { "White", "Brown", "Red", "Orange", "Yellow"
                  , "Green", "Blue", "Teal", "Purple", "Pink"
                  }
               }


  spawnMenu(start_menu_x,start_menu_y,function(menu)
    start_menu.menu = menu

    spawnMenuItem(nil,menu,0,
                      string.format("Mexica %d.%d",versonMaj,versonMin),nil)
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

      for _,f in ipairs(funs) do
        DEL_DYN(f)
      end
      start_menu = nil
      resumeGame(newGame(ps))
    end))
    spawnMenuItem(nil,menu,#start_menu.colors+1, "Start", funs[#funs])
  end)

end
