-------------------------------------------------------------------------------
-- Interacting with the players


-- Ask to choose among a bunch of text labels
function askText(p, q, labs, k)
  local funs = {}
  local menu
  local finished = false

  local function click(i)
    return function (obj,c,alt)
      if finished then return end
      if false and p and c ~= p then
        say(string.format("%s may not press %s's buttons."
                         , playerColorBB(c), playerColorBB(p)))
        return
      end
      finished = true
      menu.destroy()
      for _,f in ipairs(funs) do DEL_DYN(f) end
      k(i)
    end
  end

  menu = spawnMenu(21,15,function(menu)
    if q then spawnMenuItem(p,menu,0,q,nil) end

    for i,l in ipairs(labs) do
      funs[i] = DYN_GLOB(click(i))
      spawnMenuItem(p,menu,i,l,funs[i])
    end
  end)
end




-- Ask to choose a location on the map
function askLocs(p, q, ls, k)


  local ptrs = {}
  for i,l in ipairs(ls) do
    local pos = grid(l,2,0,0)
    ptrs[i] = { pos[1]+1.5, pos[3], -90 }
  end

  ask(p, q, ptrs, k)
end


-- Ask to choose a zone
function askZone(p, q, zs, k)
  local ps = {}
  for i,z in ipairs(zs) do
    ps[i] = zoneArrow[z]
  end

  ask(p, q, ps,k)
end


function askLeader(p, ls, k)
  local locs = {}
  for i,l in ipairs(ls) do
    local pos = GUI.leaders[l].getPosition()
    locs[i] = { pos.x - 2, pos.z, 90 }
  end
  ask(p, "Choose Leader", locs, k)
end


function ask(p, q, locs, k)

  local ql = spawnObject({
    type = "3DText",
    position = {19.5,2,15},
    rotation = {90,0,0}
  })
  ql.setValue(q)


  local markers = {}

  local finished = false

  local function cleanUp()
    for i,m in ipairs(markers) do
      _G["ask" .. i] = nil
      m.destroy()
    end
    ql.destroy()
  end

  local function btn(i,m)
    _G["ask" .. i] = function (obj,c,alt)
       if finished then return end
       if false and p and c ~= p then
          say(playerColorBB(c) .. " cannot press " .. playerColorBB(p) .. "'s buttons.")
          return --  XXX: disabled for development
       end
       finished = true
       cleanUp()
       k(i)
    end
    m.createButton({
      label = ">",
      font_size = 250,
      width = 300,
      height = 300,
      rotation = { 90, 90, 0},
      position = { 0, 0.05, -1 },
      color = playerColor(p or "Yellow"),
      font_color = textColor(p),
      click_function = "ask" .. i
    })
  end

  for ix,i in ipairs(locs) do
    markers[ix] = spawnObject({
      type = "BlockTriangle",
      position = { i[1],2,i[2] },
      scale = { 1, 1, 0.2},
      rotation = { 90, i[3], 0 },
      callback_function = function(o)
        o.setLock(true)
        o.setColorTint(playerColor(p))
        o.highlightOn({0,0,0}, 5 * 60)
        btn(ix,o)
      end
    })
  end

end





function say(x)
  printToAll(x, {1,1,1})
end


