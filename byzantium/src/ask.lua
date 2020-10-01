

function question(g,player,quest,cleanup,answer,menuOpts)
  local finished = false
  local menu     = nil
  local funs     = {}

  local function click(i)
    local f = DYN_GLOB(function (obj,clicker,alt)
      if finished then return end
      if player and clicker ~= g.controlledBy[player] then
        say(string.format("%s may not press %s's buttons."
                         , playerColorBB(clicker), playerColorBB(player)))
        return
      end
      finished = true
      cleanup()
      menu.destroy()
      for _,f in ipairs(funs) do DEL_DYN(f) end
      answer(i)
    end)
    push(funs,f)
    return f
  end

  menu = spawnMenu(24,12,function(menu)
    spawnMenuItem(p,menu,0,quest,nil)
    menuOpts(menu,click)
  end)
end


function ask(game,player,quest,options,answer)
  local toCleanActions = {}
  local toCleanCities  = {}
  local toCleanCubes   = {}

  local function cleanup()
    for _,o in ipairs(toCleanActions) do
      o.setColorTint(Color(0,0,0,0))
      o.removeButton(0)
    end

    for _,o in ipairs(toCleanCities) do
      o.removeButton(1)
    end

    for _,o in ipairs(toCleanCubes) do
      notClickableBox(o)
    end
  end

  question(game,player,quest,cleanup,answer,function(menu,click)

    -- format: { text, val }
    if options.menu == nil then options.menu = {} end
    for ix,opt in ipairs(options.menu) do
      if opt.text ~= nil then
        spawnMenuItem(player,menu,ix,opt.text,click(opt.val))
      end
    end

    -- format: { action, val }
    if options.actions == nil then options.actions = {} end
    local bg = Color(0,0,0,0)
    for _,opt in ipairs(options.actions) do
      local o = GUI.actions[opt.action]
      push(toCleanActions,o)
      local c = playerColor(player)
      c.a = 0.8
      o.setColorTint(c)
      o.createButton(
        { hover_color    = bg
        , press_color    = bg
        , color          = bg
        , click_function = click(opt.val)
        , rotation       = { 0, 180, 0 }
        , width          = 400
        , height         = 400
        })
    end

    -- format: { city, q, val }
    if options.cities == nil then options.cities = {} end
    for _,opt in ipairs(options.cities) do
      local o = GUI.cities[opt.city]
      push(toCleanCities,o)
      local s = o.getScale().x
      o.createButton(
        { label          = opt.q
        , font_size      = 300/s
        , color          = playerColor(player)
        , font_color     = playerFontColor(player)
        , click_function = click(opt.val)
        , position       = { -0.8/s, 0.5, 0.8/s }
        , rotation       = { 0, 180, 0 }
        , width          = 500/s
        , height         = 500/s
        }
      )
    end

    -- format: {}
    if options.cubes == nil then options.cubes = {} end
    local ui = GUI.players[player]
    for stat,val in pairs(options.cubes) do
      if stat == byzantium or stat == arabs then
        local fui = ui.factions[stat]
        for fstat,fval in pairs(val) do
          local o = fui[fstat]
          push(toCleanCubes,o)
          clickableBox(o, click(fval))
        end
      else
        local o = ui[stat]
        push(toCleanCubes,o)
        clickableBox(o, click(val))
      end
    end
  end)
end

