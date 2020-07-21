function question(q,answer,menuOpts)
  local finished = false
  local menu     = nil
  local funs     = {}

  local function click(i)
    local f = DYN_GLOB(function (obj,c,alt)
      if finished then return end
      if false and p and c ~= p then    -- XXX: `false` for testing
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


function askText(p,q,labs,k)
  question(q,k,function(menu,click)
    for i,l in ipairs(labs) do
      spawnMenuItem(p,menu,i,l.text,click(l.val))
    end
  end)
end

function askTextMulti(p,q,menus,k)
  question(q,k,function(menu,click)
    local i = 1

    for _,opts in ipairs(menus) do
      local lab  = opts.name
      local ents = opts.choices
      if #ents > 0 then
        i = i + 1
        if lab ~= nil then
          spawnMenuItem(p,menu,i,lab,nil)
          i = i + 1
        end
        for _,l in ipairs(ents) do
          spawnMenuItem(p,menu,i,l.text,click(l.val))
          i = i + 1
        end
      end
    end
  end)
end


function askMapLoc(p,q,spots,optionalTxt,k)
  question(q,k,function(menu,click)

    if optionalTxt ~= nil then
      spawnMenuItem(p,menu,1,optionalTxt,click(nil))
    end

    for loc,v in locsIn(spots) do
      local pos = gridToWorld(loc,1.2)
      local fun = click(loc)
      local c = playerColor(p)
      menu.createButton(
         { font_size      = 300
         , font_color     = playerFontColor(p)
         , color          = Color({r=c.r,g=c.g,b=c.b,a=0.8})
         , label          = (v == true) and "?" or (v .. "")
         , click_function = fun
         , position       = { menu_x-pos.x,piece_bridge_z, pos.z - menu_y}
         , rotation       = { 0, 180, 0 }
         , width          = 600
         , height         = 600
         }
       )
    end
  end)
end


