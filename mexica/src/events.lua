function onObjectDrop(p,o)
  local function getLoc()
    local rot = o.getRotation()
    local r   = math.floor(rot.y / 90 + 0.5)
    rot.y     = r * 90
    o.setRotation(rot)
    local dir = r + 1
    log(dir)

    local p = o.getPosition()
    local x,y = p[1], p[3]
    local row = math.floor((y + 1) / -2)+11
    local col = math.floor((x + 1) / 2)+15
    local loc = location(row,col)
    local spot = locMapLookup(globMap,loc)
    local other_spot = locMapLookup(globMap,neighbour(loc,dir))
    log(loc)
    log(neighbour(loc,dir))
    log(spot)
    log(other_spot)
    if not spot or spot.terrain ~= land or
       not other_spot or other_spot.terrain ~= land then
      o.setColorTint({1,0,0,0.8})
    end
  end
  Wait.condition(getLoc,||o.resting)
end

function onObjectPickUp(p,o)
  o.setColorTint({1,1,1,1})
end

function onLoad()
  globMap = newMap()
  newGUI(nop)
end
