
function doPlaceLeader(g,loc,owner,k)
  local s = g.playerState[owner]
  s.leader = loc
  locMapLookup(g.map,loc).leader = owner
  spawnLeader(owner, gridToWorld(loc,piece_z),function(o)
    locMapLookup(GUI.map,loc).leader = o
    k()
  end)
end

function doMoveLeader(g,from,to,k)
  local spotFrom  = locMapLookup(g.map,from)
  local spotTo    = locMapLookup(g.map,to)
  local p         = spotFrom.leader
  local s         = g.playerState[p]
  spotFrom.leader = nil
  spotTo.leader   = p
  s.leader        = to

  local uiFrom    = locMapLookup(GUI.map,from)
  local uiTo      = locMapLookup(GUI.map,to)
  local ui        = uiFrom.leader
  uiFrom.leader   = nil
  uiTo.leader     = ui

  local z = piece_z
  if spotTo.entity and
     spotTo.entity.entity == bridge then z = piece_bridge_z end
  ui.setPositionSmooth(gridToWorld(to,z), false, false)
  Wait.condition(k, ||ui.resting)
end

-- Just place a canal on the map, does not update counters
function doPlaceCanal(g,loc,k)
  locMapInsert(g.map,loc,terCanal())
  spawn1x1(gridToWorld(loc,piece_z),function(o)
    local ui = locMapLookup(GUI.map,loc)
    ui.terrain = o
    k()
  end)
end

function doBuildCanal1x1(g)
  g.canal1 = g.canal1 - 1
  counterChange(GUI.canal1,g.canal1)
end

function doBuildCanal2x1(g)
  g.canal2 = g.canal2 - 1
  counterChange(GUI.canal2,g.canal2)
end

function doBuildBridge(g,loc,dir,k)
  locMapLookup(g.map,loc).entity = entBridge(dir)
  local function markFoundation(d)
    local spot = locMapLookup(g.map,neighbour(loc,d))
    spot.bridgeFoundation = spot.bridgeFoundation + 1
  end
  if dir == east_west then
    markFoundation(east)
    markFoundation(west)
  else
    markFoundation(north)
    markFoundation(south)
  end
  g.bridges = g.bridges - 1
  spawnBridge(gridToWorld(loc,piece_z),dir,function(o)
    locMapLookup(GUI.map,loc).entity = o
    k()
  end)
end


function doBuildTemple(g,p,loc,level,k)
  local s = g.playerState[p]
  local n = s.temples[level]
  s.temples[level] = n - 1
  locMapLookup(g.map,loc).entity = entTemple(p,level)

  spawnTemple(p, gridToWorld(loc,piece_z),level,function(o)
    editPlayerTemple(p,level,s.temples[level])
    locMapLookup(GUI.map,loc).entity = o
    k()
  end)
end

