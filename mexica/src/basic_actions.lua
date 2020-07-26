
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
  spawnBridge(gridToWorld(loc,piece_bridge_base_z),dir,function(o)
    locMapLookup(GUI.map,loc).entity = o
    k()
  end)
end

function doMoveBridge(g,from,to,dir,k)
  local function changeFoundation(l,d,x)
    local spot = locMapLookup(g.map,neighbour(l,d))
    spot.bridgeFoundation = spot.bridgeFoundation + x
  end

  local spotFrom = locMapLookup(g.map,from)
  if spotFrom.entity.direction == east_west then
    changeFoundation(from,east,-1)
    changeFoundation(from,west,-1)
  else
    changeFoundation(from,north,-1)
    changeFoundation(from,south,-1)
  end
  spotFrom.entity = nil

  local spotTo = locMapLookup(g.map,to)
  spotTo.entity = entBridge(dir)
  if spotTo.entity.direction == east_west then
    changeFoundation(to,east,1)
    changeFoundation(to,west,1)
  else
    changeFoundation(to,north,1)
    changeFoundation(to,south,1)
  end

  local ui = locMapLookup(GUI.map,from)
  local b  = ui.entity
  ui.entity = nil
  locMapLookup(GUI.map,to).entity = b
  b.setPositionSmooth(gridToWorld(to,piece_z),false,false)
  local rotation = (dir == east_west) and { 0, 0, 0 } or { 0, 90, 0 }
  b.setRotation(rotation)

  Wait.condition(k,||b.resting)
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

function doScoreVP(g,p,n)
  local s = g.playerState[p]
  s.VP = s.VP + n
  editPlayerVP(p,s.VP)
  say(string.format("%s scored %d VP",playerColorBB(p),n))
end

function doEstablish(g,p,loc,i,spots,k)
  local district  = g.districts[i]
  local founderVP = math.ceil(district / 2)
  local otherVP   = math.ceil(founderVP / 2)

  for l,_ in locsIn(spots) do
    local spot = locMapLookup(g.map,l)
    spot.inDistrict = true
    local q = spot.leader
    if q ~= nil then
      doScoreVP(g,q, q == p and founderVP or otherVP)
    end
    if locationSame(l,loc) then
      spot.entity = entDistrict(district)
    end
  end

  g.districts[i] = nil
  push(g.established,spots)

  local ui = GUI.districts[i]
  GUI.districts[i] = nil
  locMapLookup(GUI.map,loc).entity = ui
  ui.setPositionSmooth(gridToWorld(loc,0),false,false)

  Wait.condition(k,||ui.resting)
end


function doSaveAP(g,p,n)
  g.saveAction = g.saveAction - n
  local s = g.playerState[p]
  s.savedAP = s.savedAP + n
  counterChange(GUI.saveAction,g.saveAction)
  editPlayerAP(p,s.savedAP)
end

function doRestoreAP(g,p)
  local s = g.playerState[p]
  s.savedAP = s.savedAP - 1
  s.turnAP = s.turnAP + 1
  g.saveAction = g.saveAction + 1
  editPlayerAP(p,s.savedAP)
  counterChange(GUI.saveAction,g.saveAction)
end

function doEndAge(g,k)
  g.phase = age2
  g.districts = g.districtsNext
  g.districtsNext = nil

  local newTs = {3,2,2,2}
  for p,s in pairs(g.playerState) do
    for i = 1,4 do
      s.temples[i] = s.temples[i] + newTs[i]
      editPlayerTemple(p,i,s.temples[i])
    end
  end

  editPhase(g.phase)
  spawnUnbuilt(g,k)
end

function doScoreDistrict(g,spots)
  local size = 0

  local influence = {}
  for _,p in ipairs(g.players) do
    influence[p] = 0
  end

  for l,_ in locsIn(spots) do
    size = size + 1
    local ent = locMapLookup(g.map,l).entity
    if ent ~= nil and ent.entity == temple then
      influence[ent.owner] = influence[ent.owner] + ent.level
    end
  end

  local order = {}
  for p,x in pairs(influence) do
    push(order, { player = p, influence = x })
  end
  table.sort(order, |x,y| x.influence > y.influence)

  local curScore = order[1].influence
  local award = size
  local prevAward = award
  for i,ord in ipairs(order) do
    if ord.influence == 0 then break end

    if ord.influence == curScore then
      doScoreVP(g,ord.player,prevAward)
    else
      if i <= 3 then
        doScoreVP(g,ord.player,award)
        curScore = ord.influence
        prevAward = award
      else
        prevAward = 0
      end
    end
    award = math.ceil(award/2)
  end
end

function doEndGame(g)
  g.phase = finished
  editPhase(g.phase)
end

function doRemoveDistricts(g,ds)
  for _,i in ipairs(ds) do
    g.districts[i] = nil
    GUI.districts[i].destroy()
  end
end

