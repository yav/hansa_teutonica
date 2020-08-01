function newGUI()

  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end

  GUI =
    { board = nil
    }

  local q = actQ()
  q.enQ(||spawnMap(q.next))
  q.enQ(function()
    for i,d in ipairs(allDirections) do
      local l = location(dirDRow[d], dirDCol[d])
      -- spawnTrain("Orange",gridToWorld(l,train_z),||nil)
      -- spawnMeeple(gridToWorld(l,meeple_z),||nil)
      spawnDisc("Red",gridToWorld(l,disc_z),||nil)
    end
  end)

end


function spawnMap(k)
  o = spawnObject(
    { type      = "Custom_Tile"
    , sound     = false
    , position  = { 0.25, board_z, 0.5 }
    , scale     = { 17.7, 1, 17.7 }
    , rotation  = { 0, 180, 0 }
    , callback_function = function(o)
        o.setLock(true)
        GUI.board = o
        k()
      end
    }
  )

  o.setCustomObject(
    { image = board_url
    }
  )

  return o
end


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






function gridToWorld(loc,z)
  local side = 1
  local w    = 2   * side * math.cos(math.pi/6)
  local h    = 1.5 * side
  local r,c = loc.row, loc.col
  local x = (c + r/2) * w
  local y = r * h
  return { x, z, y }
end



