local zoneLoc = { {12,-9}, {-23.5,-9}, {-23.5,17}, {12,17}, {12,5.5} }
local zoneArrow = { {10,-7,135}, {-20,-7,-135},
                    {-19.5,14,-45}, {10,14,45},
                    {9,4.7,90} }


local map_data = {
  { { {1,7}, 1, "Archdiocese",         {{1,6}, {2,6} }},
    { {4,4}, 2, "Protestant District", {{3,3}, {5,4}, {3,5}} },
    { {8,4}, 2, "Irish District",      {{7,3}, {9,4}, {7,5}} },
    { {6,8}, 3, "Jewish District",     {{5,8}, {5,7}, {6,7}} }
  },
  { { {8,8}, 1, "Seat of Government",       {{7,7}, {8,7}} },
    { {5,3}, 2, "Municipal Administration", {{4,3}, {6,2}, {6,4}} },
    { {3,7}, 2, "National Administration",  {{2,7}, {4,6}, {4,8}} },
    { {1,3}, 3, "Millitary Administration", {{1,4}, {2,4}, {2,3}} }
  },
  { { {9,2}, 1, "Public Market",      {{8,3}, {9,3}} },
    { {6,5}, 2, "Buisiness District", {{7,4}, {5,5}, {7,6}} },
    { {2,5}, 2, "Rich District",      {{3,4}, {1,5}, {3,6}} },
    { {4,1}, 3, "Port District",      {{4,2}, {5,2}, {5,1}} }
  },
  { { {2,1}, 1, "Social Club",            {{2,2}, {3,2}} },
    { {7,2}, 2, "Entertainment District", {{6,1}, {8,2}, {6,3}} },
    { {5,6}, 2, "Academic District",      {{4,5}, {6,6}, {4,7}} },
    { {9,6}, 3, "Arts District",          {{8,6}, {8,5}, {9,5}} }
  },
}





-------------------------------------------------------------------------------
-- Static map info

local finalAge = 4
local hub = {}            -- maps building sites to actions
local spotByColor = {}    -- all spots of a given color (for setup)
local allLocs = {}

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



