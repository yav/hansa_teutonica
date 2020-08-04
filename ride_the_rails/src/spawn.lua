function newGUI(g,k)

  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end

  GUI =
    { board = nil
    , map   = locMapEmpty()
    }

  local sem = newSem()
  for l,spot in locsIn(g.map.locations) do
    sem.up(); spawnHex(spot,gridToWorld(l,board_z),sem.down)
    local ui = { trains = {}, passenger = nil }
    locMapInsert(GUI.map, l, ui)
    if spot.passenger then
      sem.up(); spawnPassengerAt(l,sem.down)
    end
    local q = actQ()
    sem.up()
    for c,_ in pairs(spot.trains) do
      q.enQ(||spawnTrainAt(l,c,q.next))
    end
    q.enQ(sem.down)
  end

  sem.wait(k)
end

function gridToWorld(loc,z)
  local side = 1
  local w    = 2   * side * math.cos(math.pi/6)
  local h    = 1.5 * side
  local r,c  = loc.row, loc.col
  local x    = (c + r/2) * w
  local y    = r * h
  return Vector(x,z,y)
end



--------------------------------------------------------------------------------


function spawnPassengerAt(loc,k)
  local pos = gridToWorld(loc,meeple_z)
  pos.z = pos.z + 0.7
  spawnMeeple(pos,function(o)
    locMapLookup(GUI.map,loc).passenger = o
    k(o)
  end)
end


function spawnTrainAt(loc,company,k)
  local pos    = gridToWorld(loc,train_z)
  spawnTrain(company,pos,function(o)
    local ui     = locMapLookup(GUI.map,loc)
    local others = ui.trains
    push(others,o)
    local sc = 1.1 - 0.12 * #others
    local basex = 0
    if #others > 3 then basex = -0.25 end
    for i,t in ipairs(others) do
      t.setScale({sc,sc,sc})
      local x = basex + pos.x + 1.7 * sc * math.floor((i-1) / 3)
      local z = pos.z - 0.5 * sc * ((i - 1) % 3)
      t.setPosition(Vector(x, pos.y, z))
    end
    k(o)
  end)
end




--------------------------------------------------------------------------------
-- Just the shapes

function spawnTrain(companyName, loc, k)
  local s = 0.75
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , scale        = {s,s,s}
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        local c = companyColor(companyName)
        o.setColorTint(c)
        o.setName(string.format("%s train", colorNote(c,companyName)))
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = train_url
    , material = 1
    }
  )

  return o
end


function spawnMeeple(loc, k)
  local s = 1
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , scale        = {s,s,s}
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        o.setName("Passenger")
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = meeple_url
    , material = 1
    }
  )

  return o
end


function spawnDisc(p, loc, k)
  local s = 1
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , scale        = {s,s,s}
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        local c = playerColor(p)
        o.setColorTint(c)
        o.setName(string.format("%s player", playerColorBB(p)))
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = disc_url
    , material = 1
    }
  )

  return o
end

function spawnHex(spot, loc, k)
  local s = 0.5
  if spot.url == nil then k() end
  local o = spawnObject(
    { type         = "Custom_Token"
    , position     = loc
    , rotation     = { 0, 180, 0 }
    , scale        = {s,s,s}
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        local lab = "Plains"
        if spot.terrain == terrainCity then
          lab = "City"
          if spot.bonus > 0 then
            lab = string.format("%s %s$%d"
                               , lab
                               , spot.bonusType == bonusRecurring and "∞ " or ""
                               , spot.bonus )
          end
        elseif spot.terrain == terrainMountains then lab = "Mountains"
        end
       o.setName(lab)
        k(o)
      end
    }
  )
  local url
  o.setCustomObject(
    { image = spot.url
    }
  )

  return o
end

