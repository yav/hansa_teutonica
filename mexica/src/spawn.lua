
function newGUI(g, k)
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
  GUI = {}

  local sem = newSem()
  sem.up(); spawnMap(g, sem.down)
  sem.up(); spawn1x1Counter(g.canal1, sem.down)
  sem.up(); spawn2x1Counter(g.canal2, sem.down)
  sem.up(); spawnBridgeCounter(g.bridges, sem.down)
  sem.up(); spawnAPCounter(g.saveAction, sem.down)
  sem.up(); spawnUnbuilt(g.districts[g.age],sem.down)

  sem.wait(k)
end

function spawnBoard(k)
  local scale = 16
  local sem = newSem()

  GUI.board = spawnObject(
    { type      = "Custom_Tile"
    , position  = { board_x, 1, board_y }
    , rotation  = { 0, 180, 0 }
    , scale     = { scale, 1, scale }
    , sound     = false
    , callback_function = function(o)
      o.setLock(true)
      o.grid_projection = true
      k(o)
    end
    }
  )
  GUI.board.setCustomObject(
    { image = map_url
    }
  )
end

function spawnMap(g, k)
  local sem = newSem()
  sem.up()
  spawnBoard(sem.down)
  for loc,spot in locsIn(g.map) do
    sem.up()
    spawnLoc(g,loc,spot,sem.down)
  end
  sem.wait(k)
end


function spawnLoc(g,loc,spot,k)
  local q  = actQ()
  local z  = piece_z
  local ui = {}
  local function saveUI(x) return function(y)
      ui[x] = y
      q.next()
    end
  end

  if spot.terrain == canal and not locMapLookup(g.mapEdges,loc) then
    local wc = gridToWorld(loc,piece_z)
    q.enQ(||spawn1x1(wc,saveUI("terrain")))
  end

  local ent = spot.entity

  if ent then

    if ent.entity == bridge then
      q.enQ(||spawnBridge( gridToWorld(loc,piece_z)
                         , ent.direction
                         , saveUI("entity")
                         ))
      z = 1.7
    elseif ent.entity == temple then
      q.enQ(||spawnTemple( ent.owner
                         , gridToWorld(loc,piece_temple_z)
                         , ent.level
                         , saveUI("entity")
                         ))
    elseif ent.entity == district then
      q.enQ(||spawnDistrict( gridToWorld(loc,piece_z)
                           , ent.size
                           , saveUI("entity")
                           ))
    end

  end

  local leader = spot.leader
  if leader then
    q.enQ(||spawnLeader(leader, gridToWorld(loc,z), saveUI("leader")))
  end

  q.enQ(k)
end


--------------------------------------------------------------------------------
function counterLabel(o,x,y,msg,l)
  local bg = {0,0,0, 0.8}
  local fg = {1,1,1}
  local lab = string.format("%s: %d", msg, l)
  o.setName(msg)
  o.createButton(
    { label       = l .. ""
    , color       = bg
    , hover_color = bg
    , press_color = bg
    , font_color  = fg
    , rotation    = { 0, 180, 0 }
    , position    = { x, y, 0 }
    , font_size   = 300
    , width       = 700
    , height      = 700
    , tooltip     = lab
    , click_function  = "nop"
    }
  )
end

function counterChange(o,n)
  o.editButton(
    { index = 0
    , label = n .. ""
    , tooltip = string.format("%s: %s", o.getName(), n)
    }
  )
end


function spawn1x1Counter(n,k)
  spawn1x1(gridToWorld(location(1,16),piece_z),function(o)
    counterLabel(o, 0, 0.2, "Single Canals",n)
    GUI.canal1 = o
    k(o)
  end)
end

function spawn2x1Counter(n,k)
  spawn2x1(gridToWorld(location(1,19),piece_z),function(o)
    counterLabel(o, 1, 0.2, "Double Canals", n)
    GUI.canal2 = o
    k(o)
  end)
end

function spawnBridgeCounter(n,k)
  spawnBridge(gridToWorld(location(1,21),piece_z),east_west,function(o)
    GUI.bridges = o
    counterLabel(o, 0, 0.7, "Bridges", n)
    k(o)
  end)
end

function spawnAPCounter(n,k)
  spawn1x1(gridToWorld(location(1,23),piece_z),function(o)
    o.setColorTint({0.5,0,5,0.5})
    GUI.saveAction = o
    counterLabel(o, 0, 0.2, "Save AP", n)
    k(o)
  end)
end

function spawnUnbuilt(ds,k)
  GUI.districts = {}
  local sem = newSem()
  for i,n in pairs(ds) do
    local row = 15 + math.floor((i-1) / 4)
    local col = 20 + math.floor((i-1) % 4)
    sem.up()
    spawnDistrict(gridToWorld(location(row,col),piece_z),n,function(o)
      GUI.districts[i] = o
      sem.down()
    end)
  end
  sem.wait(k)
end




--------------------------------------------------------------------------------

function spawnDistrict(loc,size,k)
  local p1 = size
  local p2 = math.ceil(p1/2)
  local p3 = math.ceil(p2/2)
  local lab = "District"
  spawnObject(
    { type = "BlockSquare"
    , position = { loc[1], 0, loc[3] }
    , snap_to_grid = true
    , sound = false
    , callback_function = function(o)
        o.setLock(true)
        o.setName(lab)
        o.createButton(
          { label     = string.format("[FFFF00]%d[-]\n%d %d",p1,p2,p3)
          , font_size = 300
          , font_color= {1,1,1}
          , color     = {0,0,0}
          , hover     = {0,0,0}
          , height    = 700
          , width     = 700
          , position  = { 0, piece_z, 0 }
          , rotation  = {0, 180, 0}
          , tooltip   = lab
          , click_function = "nop"
          }
        )
        k(o)
      end
    }
  )
end

function spawnLeader(p, loc, k)
  spawnObject(
    { type          = "PlayerPawn"
    , position      = loc
    , sound         = false
    , snap_to_grid  = true
    , callback_function = function(o)
        o.setLock(true)
        o.setColorTint(playerColor(p))
        local lab = string.format("%s leader", playerColorBB(p))
        o.setName(lab)
        k(o)
      end
    }
  )
end


function spawnBridge(loc, dir, k)
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , rotation     = (dir == east_west) and { 0, 0, 0 } or { 0, 90, 0 }
    , snap_to_grid = true
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        o.setColorTint({0.6,0.3,0})
        o.setName("Bridge")
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = model_bridge_url
    , material = 1
    }
  )
end


function spawnTemple(p, loc,level,k)
  local h = 0.6
  local q = actQ()
  local obj

  local function floor(n)
    local loc1 = Vector(loc[1],loc[2] + (n - 1) * h,loc[3])
    local s = 1.75 - (4-level + n) * 0.25
    local o = spawnObject(
      { type         = "BlockSquare"
      , scale        = { s,1,s }
      , position     = loc1
      , snap_to_grid = true
      , sound        = false
      , callback_function = function(o)
          o.setLock(true)
          o.setColorTint(playerColor(p))
          if n > 1 then obj.addAttachment(o) end
          q.next()
        end
      }
    )
    if n == 1 then obj = o end
  end

  for i = 1,level do
    q.enQ(||floor(i))
  end

  q.enQ(function()
    local x = obj.getScale()[1]
    local lab = string.format("%s Temple, Level %d", playerColorBB(p), level)
    obj.setName(lab)
    obj.createButton(
      { label = level .. ""
      , font_size = 300 / x
      , font_color = playerFontColor(p)
      , color = playerColor(p)
      , rotation = { 0, 180, 0 }
      , width = 400 / x
      , height = 400 / x
      , position = {0,level * h + 0.1,0}
      , click_function = "nop"
      , tooltip = lab
      }
    )
    q.next()
  end)

  q.enQ(||k(obj))
end


function spawn1x1 (loc,k)
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , snap_to_grid = true
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        o.setName("Canal")
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = model_1x1_url
    , diffuse  = water_url
    , material = 3
    }
  )
  return o
end

function spawn2x1 (loc,k)
  local scale = 1
  local o = spawnObject(
    { type         = "Custom_Model"
    , scale        = { scale, 1, scale }
    , position     = loc
    , snap_to_grid = true
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = model_2x1_url
    , diffuse  = water_url
    , material = 3
    }
  )
  return o
end



