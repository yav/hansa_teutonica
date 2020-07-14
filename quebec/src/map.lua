



-------------------------------------------------------------------------------
-- Static map info

function locId(l) return l[1] .. "_" .. l[2] end

function staticMapSetup(k)
  local sem = newSem()
  for i,xs in ipairs(map_data) do
     for j,x in ipairs(xs) do
       sem.up()
       setupLocation(i,j,x,sem.down)
     end
  end
  sem.wait(k)
end

function setupLocation(i,j,info, k)

  local name = info[3]

  local function act(gs,loc)
    local p = gsCurPlayer(gs)
    say(playerColorBB(p) .. " uses the " .. name)
    local fun = actions[i][j]
    fun(gs,loc)
  end

  local spots = {}
  for x,l in ipairs(info[4]) do
    local id = locId(l)
    spots[x] = id
    allLocs[id] = l
  end

  local id = locId(info[1])
  allLocs[id] = info[1]
  local me = {
    id = id,
    loc = info[1],
    requires = info[2],
    act = act,
    name = name,
    spots = spots,
    color = i
  }

  for _,x in ipairs(me.spots) do
    hub[x] = me
  end

  local objs = spotByColor[i]
  if not objs then
    objs = {}
    spotByColor[i] = objs
  end

  push(objs, me)

  local t = spawnObject({
    type = "Custom_Tile",
    position = grid(id,1.1,0,0),
    scale = { 1.35, 1.35, 1.35 },
    rotation = { 0, 180, 0 },
    callback_function = function(o)
      o.setLock(true)
      o.setName(name .. ", " .. zoneName[me.color])
      local d = "Contribution:\n  " .. me.requires .. " active worker"
      if me.requires > 1 then d = d .. "s" end
      d = d .. "\n\nActivation:"
      for _,ln in ipairs(actionD[i][j]) do
         d = d .. "\n  " .. ln
      end
      o.setDescription(d)
      k()
    end
  })
  t.setCustomObject({
    type = 2,
    thickness = 0.1,
    image = map_tile_url[i][j]
  })

end



function grid(pid,z,a,b)
  local p = allLocs[pid]
  local x0 = -24
  local y0 = 15.1

  local y = p[2]
  local x = p[1] + y%2/2

  local dx = 3.02
  local dy = -2.666
  local extra = y%2 / 2
  local ox = a/2
  local oy = b/2
  return { x0 + ox + x * dx, z, y0 + oy + y * dy }
end

function neighbours(pid)

  local pos = allLocs[pid]
  if not pos then return {} end

  local x = pos[1]
  local y = pos[2]

  local even = math.floor(y/2) == y/2
  local d = even and -1 or 1
  local ls = { {x-1,y}, {x+1,y}, {x,y-1}, {x,y+1},{x+d,y-1},{x+d,y+1}}
  local r = {}
  for i,l in ipairs(ls) do
    local i = locId(l)
    if allLocs[i] then push(r,i) end
  end
  return r
end



