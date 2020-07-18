
function newGUI(k)
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
  GUI = {}

  local sem = newSem()
  sem.up(); spawnMap(sem.down)
  sem.up(); spawn2x1( {0,1.2,0}, sem.down )

  sem.wait(k)
end

function spawnMap(k)
  local scale = 16
  local sem = newSem()

  GUI.board = spawnObject(
    { type      = "Custom_Tile"
    , position  = { board_x, 1, board_y }
    , rotation  = { 0, 180, 0 }
    , scale     = { scale, 1, scale }
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


function spawn1x1 (loc,k)
  local scale = 1
  local o = spawnObject(
    { type  = "Custom_Model"
    , scale = { scale, 1, scale }
    , position = loc
    , callback_function = function(o)
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
    { type  = "Custom_Model"
    , scale = { scale, 1, scale }
    , position = loc
    , callback_function = function(o)
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

function toGrid(loc)
end


