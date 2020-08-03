function question(g,p,q,answer,menuOpts)
  local finished = false
  local menu     = nil
  local funs     = {}

  local function click(i)
    local f = DYN_GLOB(function (obj,c,alt)
      if finished then return end
      if p and c ~= g.controlledBy[p] then
        say(string.format("%s may not press %s's buttons."
                         , playerColorBB(c), playerColorBB(p)))
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

  menu = spawnMenu(menu_x,menu_y,function(menu)
    spawnMenuItem(p,menu,0,q,nil)
    menuOpts(menu,click)
  end)
end


function askText(g,p,q,labs,k)
  question(g,p,q,k,function(menu,click)
    for i,l in ipairs(labs) do
      spawnMenuItem(p,menu,i,l.text,click(l.val))
    end
  end)
end

function askMapLoc(g,p,q,spots,txt,k)
  question(g,p,q,k,function(menu,click)

    for i,l in ipairs(txt) do
      spawnMenuItem(p,menu,i,l.text,click({ tag = ans_menu, value = l.val }))
    end

    for loc,v in locsIn(spots) do
      local spot = locMapLookup(g.map.locations,loc)
      local hi   = map_question_z
      if spot and (spot.passenger or trainNum(spot) > 0) then
        hi = map_question_hi_z
      end
      local pos = gridToWorld(loc,hi)
      local fun = click({ tag = ans_location, location = loc })
      local c = playerColor(p)
      menu.createButton(
         { font_size      = 300
         , font_color     = playerFontColor(p)
         , color          = Color({r=c.r,g=c.g,b=c.b,a=0.8})
         , label          = (v == true) and "?" or (v .. "")
         , click_function = fun
         , position       = { menu_x - pos.x, pos.y, pos.z - menu_y }
         , rotation       = { 0, 180, 0 }
         , width          = 600
         , height         = 600
         }
       )
    end
  end)
end


