function doPlaceActive(g,spot,w,k)
  doChangeActive(g,w.owner,w.shape,-1)
  doPlaceWorker(g,spot,w,k)
end

function doPlacePassive(g,spot,w,k)
  doChangePassive(g,w.owner,w.shape,-1)
  doPlaceWorker(g,spot,w,k)
end

function doChangeActive(g,p,t,x)
  if x == 0 then return end

  local s = g.playerState[p]
  s.active[t] = s.active[t] + x
  setLabel(GUI.player[p].active[t], s.active[t] .. "")
  local verb = x > 0 and "gained" or "lost"
  local amt = x
  if amt < 0 then amt = - amt end
  local suff = "s"
  if amt == 1 then suff = "" end
  say(playerColorBB(p) .. " " .. verb .. " " .. amt ..
                                " active " .. workerName(t) .. suff .. ".")
end

function doChangePassive(g,p,t,x)
  if x == 0 then return end

  local s = g.playerState[p]
  s.passive[t] = s.passive[t] + x
  setLabel(GUI.player[p].passive[t], s.passive[t] .. "")
  local verb = x > 0 and "gained" or "lost"
  local amt = x
  if amt < 0 then amt = - amt end
  local suff = "s"
  if amt == 1 then suff = "" end
  say(playerColorBB(p) .. " " .. verb .. " " .. amt ..
                                " passive " .. workerName(t) .. suff .. ".")
end

function doMoveWorker(g,from,to,k)
  local w = g.map.edges[from.edge].stops[from.stop].worker
  doRemoveWorker(g,from)
  doPlaceWorker(g,to,w,k)
end

function doRemoveWorker(g,spot)
  local edge = g.map.edges[spot.edge]
  local stop = edge.stops[spot.stop]
  local w = stop.worker
  stop.worker = nil
  GUI.edge[spot.edge].stops[spot.stop].destroy()

  local thing = stopName(stop.type)
  say(playerColorBB(w.owner) .. " removed a " .. workerName(w.shape) ..
        " from the " .. thing .. " between " .. edge.from .. " and "
        .. edge.to .. ".")
end

function doPlaceWorker(g,spot,w,k)
  local edge = g.map.edges[spot.edge]
  local stop = edge.stops[spot.stop]
  stop.worker = w
  local thing = stopName(stop.type)
  say(playerColorBB(w.owner) .. " placed a " .. workerName(w.shape) ..
        " on the " .. thing .. " between " .. edge.from .. " and "
        .. edge.to .. ".")
  spawnWorker(w, { stop.x, boardPieceZ, stop.y }, function(o)
    GUI.edge[spot.edge].stops[spot.stop] = o
    k()
  end)
end


function doScorePoints(g,p,n)
  if n == 0 then return end
  local s = g.playerState[p]
  s.score = s.score + n
  local ui = GUI.player[p]
  ui.score.setValue(s.score .. " VP")

  if s.score >= 20 then g.endGame = true end

  say(playerColorBB(p) .. " scored " .. n .. " VP.")
end

function doCityBecameFull(g)
  local n = g.map.fullCities
  g.map.fullCities = n + 1
  if g.map.fullCities >= g.map.fullCityLimit then g.endGame = true end
  GUI.fullCities.setPositionSmooth(fullCityLoc(g.map),false,false)
end

function doFillOffice(g,n,w,k)
  local node = g.map.nodes[n]
  local x = node.x
  for i,off in ipairs(node.offices) do
    if not off.worker then
      if i == #node.offices then
        doCityBecameFull(g)
      end
      off.worker = w
      say(playerColorBB(w.owner) .. " establised an office in " .. node.name)
      checkWinRace(g,w.owner)
      spawnWorker(w, {x, boardPieceZ, node.y}, function(o)
        GUI.node[n].offices[i] = o
        local p = w.owner
        if off.vp > 0 then doScorePoints(g,p,off.vp) end
        if i == 1 or node.offices[i - 1].worker.owner ~= p then
          -- we are the new right-most
          gainForeignBuilds(g,w.owner,node)
        end
        k()
      end)
      return
    else
      x = x + GUI.officeWidth[off.shape]
    end
  end
end

function doBuildOffice(g,n,e,k)
  local edge = g.map.edges[e]
  local office = nextFreeOffice(g,n)
  for i,stop in ipairs(edge.stops) do
    local w = stop.worker
    if stop.worker.shape == office.shape then
      doRemoveWorker(g,{edge=e,stop=i})
      doFillOffice(g,n,w,k)
      break
    end
  end
end

function checkWinRace(g,p)
  local place = g.raceAward
  if place > #raceAward then return end
  if not officeConnection(g,p,g.map.raceFrom,g.map.raceTo) then return end

  local suff = { "st","nd","rd" }
  say(playerColorBB(p) .. " is " .. place .. suff[place] ..
        " to connect " .. g.map.raceFrom .. " and " .. g.map.raceTo)
  doScorePoints(g,p,raceAward[place])
  g.raceAward = place + 1
end


function doAddExtra(g,n,w,k)
  local node = g.map.nodes[n]
  local x = node.x
  for i,w in ipairs(node.extraOffices) do
    x = x - GUI.officeWidth[w.shape]
  end
  x = x - GUI.officeWidth[w.shape]

  say(playerColorBB(w.owner) .. " establised an expansion office in "
                                                          .. node.name)
  push(node.extraOffices, w)
  checkWinRace(g,w.owner)

  spawnWorker(w,{x,boardPieceZ,node.y}, function(o)
    push(GUI.node[n].extraOffices,o)
    k()
  end)
end





function doPlaceBouns(g,p,b,e,k)
  local edge = g.map.edges[e]
  edge.bonus = b
  spawnBonus(b, {edge.x,boardPieceZ,edge.y}, edge.rotation, function(o)
    GUI.edge[e].bonus = o
    say(playerColorBB(p) .. " placed a bonus token between " ..
          g.map.nodes[edge.from].name .. " and " ..
          g.map.nodes[edge.to].name)
    k()
  end)
end

function doTakeBonus(g,p,e,k)
  local map = g.map
  local edge = map.edges[e]

  local x = GUI.edge[e].bonus
  GUI.edge[e].bonus = nil
  x.destroy()

  local s = g.playerState[p]
  push(s.plates, edge.bonus)
  local b = edge.bonus
  edge.bonus = nil

  if g.nextBonus > #g.bonus then
    g.endGame = true
  else
    local n = g.nextBonus
    push(s.turnReplaceBonus, g.bonus[n])
    g.nextBonus = n + 1
    updatePlateCounter(g)
  end
  spawnPlate(g,p,#s.plates,b,function()
    say(playerColorBB(p) .. " picked up the bonus token between " ..
          g.map.nodes[edge.from].name .. " and " ..
          g.map.nodes[edge.to].name)
    k()
  end)
end

function doUseUpBonus(g,p,i)
  local s = g.playerState[p]
  local ui = GUI.player[p]
  ui.plates[i].destroy()
  for j = (i+1),#s.plates do
    s.plates[j-1] = s.plates[j]
    ui.plates[j-1] = ui.plates[j]
  end
  s.plates[#s.plates] = nil
  ui.plates[#ui.plates] = nil
  s.finishedPlates = s.finishedPlates + 1
  ui.finishedPlates.setValue(s.finishedPlates .. "")
end


function doUpgradeAction(g,p)
  local s = g.playerState[p]
  if s.actionLevel == #actionLevelMap then return end

  say(playerColorBB(p) .. " upgraded their actions.")
  local curActNum = actionLevelMap[s.actionLevel]
  s.actionLevel = s.actionLevel + 1
  local newActNum = actionLevelMap[s.actionLevel]
  s.turnActions = s.turnActions + (newActNum - curActNum)

  GUI.player[p].actionLevel[s.actionLevel].destroy()
  doChangeActive(g,p,trader,1)
end


function doUpgradeBag(g,p)
  local s = g.playerState[p]
  if s.bagLevel == #bagLevelMap then return end

  say(playerColorBB(p) .. " upgraded their bag.")
  s.bagLevel = s.bagLevel + 1

  GUI.player[p].bagLevel[s.bagLevel].destroy()
  doChangeActive(g,p,trader,1)
end


function doUpgradeBuilding(g,p)
  local s = g.playerState[p]
  if s.buildingLevel == #buildingLevelMap then return end

  say(playerColorBB(p) .. " upgraded their building.")
  s.buildingLevel = s.buildingLevel + 1

  GUI.player[p].buildingLevel[s.buildingLevel].destroy()
  doChangeActive(g,p,trader,1)
end

function doUpgradeKey(g,p)
  local s = g.playerState[p]
  if s.keyLevel == #keyLevelMap then return end

  say(playerColorBB(p) .. " upgraded their keys.")
  s.keyLevel = s.keyLevel + 1

  GUI.player[p].keyLevel[s.keyLevel].destroy()
  doChangeActive(g,p,trader,1)
end

function doUpgradeBook(g,p)
  local s = g.playerState[p]
  if s.bookLevel == #bookLevelMap then return end

  say(playerColorBB(p) .. " upgraded their books.")
  s.bookLevel = s.bookLevel + 1

  GUI.player[p].bookLevel[s.bookLevel].destroy()
  doChangeActive(g,p,merchant,1)
end

function doAddInvest(g,p,i,k)
  local w = { owner = p, shape = merchant }
  g.map.endGameInvest[i] = w
  spawnInvest(g.map, w, i, k)
end

-- swap i and i-1. assumes i > 1
function doSwap(g,n,i,k)
  local node = g.map.nodes[n]
  local off1 = node.offices[i-1]
  local off2 = node.offices[i]
  local w = off1.worker
  off1.worker = off2.worker
  off2.worker = w

  if i == #node.offices or not node.offices[i + 1].worker then
    -- only really matters if it's the current player
    gainForeignBuilds(g,w.owner,node)
  end

  local o1 = GUI.node[n].offices[i-1]
  local o2 = GUI.node[n].offices[i]
  local p1 = o1.getPosition()
  local p2 = o2.getPosition()
  o1.destroy()
  o2.destroy()

  local sem = newSem()
  sem.up()
  GUI.node[n].offices[i-1] = spawnWorker(off1.worker,p1,sem.down)
  sem.up()
  GUI.node[n].offices[i]   = spawnWorker(off2.worker,p2,sem.down)
  sem.wait(k)
end




