function question(qs,answer,menuOpts)
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
    for i,q in ipairs(qs) do
      spawnMenuItem(p,menu,i-1,q,nil)
      menuOpts(menu,#qs-1,click)
    end
  end)
end



-- Ask to choose among a bunch of text labels
function askTextMany(p, qs, labs, k)
  question(qs,k,function(menu,n,click)
    for i,l in ipairs(labs) do
      spawnMenuItem(p,menu,n + i,l.text,click(l.val))
    end
  end)
end

function askText(p,q,labs,k)
  askTextMany(p,q and {q} or {},labs,k)
end


function askMapLoc(p,q,spots,optionalTxt,k)
  question({q},k,function(menu,n,click)

    if optionalTxt ~= nil then
      spawnMenuItem(p,menu,n + 1,optionalTxt,click(nil))
    end

    for loc,_ in locsIn(spots) do
      local pos = gridToWorld(loc,1.2)
      local fun = click(loc)
      local c = playerColor(p)
      menu.createButton(
         { font_size      = 300
         , font_color     = playerFontColor(p)
         , color          = Color({r=c.r,g=c.g,b=c.b,a=0.8})
         , label          = "?"
         , click_function = fun
         , position       = { menu_x-pos.x,piece_z, pos.z - menu_y}
         , rotation       = { 0, 180, 0 }
         , width          = 600
         , height         = 600
         }
       )
    end
  end)
end


