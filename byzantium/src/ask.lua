




function question(g,player,question,answer,menuOpts)
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
      menu.destroy()
      for _,f in ipairs(funs) do DEL_DYN(f) end
      answer(i)
    end)
    push(funs,f)
    return f
  end

  menu = spawnMenu(24,12,function(menu)
    spawnMenuItem(p,menu,0,question,nil)
    menuOpts(menu,click)
  end)
end

function askText(g,player,quest,opts,answer)
  question(g,player,quest,answer,function(menu,click)
    for ix,opt in ipairs(opts) do
      spawnMenuItem(player,menu,ix,opt.text,click(opt.val))
    end
  end)
end

-- Ask text choices, if there is only one option, just select it
function askTextQuick(g,player,quest,opts,answer)
  if #opts == 1 then
    answer(opts[1].val)
    return
  end
  askText(g,player,quest,opts,answer)
end



function askAction(g,player,opts,answer)
  local label = playerColorBB(player) .. "'s turn"
  local toClean = {}

  local function cleanupAnswer(i)
    for _,o in ipairs(toClean) do
      o.setColorTint(Color(0,0,0,0))
      o.removeButton(0)
    end
    answer(i)
  end

  question(g,player,label,cleanupAnswer,function(menu,click)

    if opts.text ~= nil then
      for ix,opt in ipairs(opts.text) do
        spawnMenuItem(player,menu,ix,opt.text,click(opt.val))
      end
    end

    if opts.actions ~= nil then
      local bg = Color(0,0,0,0)
      for _,opt in ipairs(opts.actions) do
        local o = GUI.actions[opt.act]
        push(toClean,o)
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
    end
  end)
end

function askCity(g,player,quest,opts,answer)
  local toClean = {}
  local function cleanupAnswer(i)
    for _,o in ipairs(toClean) do
      o.removeButton(1)
    end
    answer(i)
  end

  question(g,player,quest,cleanupAnswer,function(menu,click)
    for _,city in ipairs(opts) do
      local o = GUI.cities[city]
      push(toClean,o)
      local s = o.getScale().x
      o.createButton(
        { label          = "?"
        , font_size      = 300/s
        , color          = playerColor(player)
        , font_color     = playerFontColor(player)
        , click_function = click(city)
        , position       = { -0.8/s, 0.5, 0.8/s }
        , rotation       = { 0, 180, 0 }
        , width          = 500/s
        , height         = 500/s
        }
      )
    end
  end)
end


