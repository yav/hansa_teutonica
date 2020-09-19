

-- Choose one of the given cities
function askCity(cityNames,k)
end

-- Ask text choices, if there is only one option, just select it
function askTextQuick(opts,k)
  if #opts == 1 then
    k(opts[1].val)
    return
  end

  -- XXX: ask question
end



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


