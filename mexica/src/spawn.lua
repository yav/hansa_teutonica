
function newGUI(g, k)
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
  GUI = {}

  local sem = newSem()
  sem.up(); spawnMap(g, sem.down)

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
  local q = actQ()
  local z = piece_z

  if spot.terrain == canal and not locMapLookup(g.mapEdges,loc) then
    local wc = gridToWorld(loc,piece_z)
    q.enQ(||spawn1x1(wc,q.next))
    -- spawn bridge
  end

  -- spawn entities
  q.enQ(k)

end


function spawnTemple(p, loc,level,k)
  local h = 0.4
  local q = actQ()
  local obj

  local function floor(n)
    local loc1 = Vector(loc[1],loc[2] + (n - 1) * h,loc[3])
    local s = 1.75 - (4-level + n) * 0.25
    local o = spawnObject(
      { type         = "BlockSquare"
      , scale        = { s,0.5,s }
      , position     = loc1
      , snap_to_grid = false
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
    local lab = string.format("%s %d Level Temple", playerColorBB(p), level)
    obj.setName(lab)
    obj.createButton(
      { label = level .. ""
      , font_size = 300 / x
      , font_color = playerFontColor(p)
      , color = playerColor(p)
      , rotation = { 0, 180, 0 }
      , width = 400 / x
      , height = 400 / x
      , position = {0,level * 0.8,0}
      , click_function = "nop"
      , tooltip = lab
      }
    )
  end)

  q.enQ(||k(obj))

end


function spawn1x1 (loc,k)
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , snap_to_grid = false
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
        -- o.setLock(true)
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



