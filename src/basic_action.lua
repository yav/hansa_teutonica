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
  print(playerColorBB(p) .. " " .. verb .. " " .. amt ..
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
  print(playerColorBB(p) .. " " .. verb .. " " .. amt ..
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
  print(playerColorBB(w.owner) .. " removed a " .. workerName(w.shape) ..
        " from the " .. thing .. " between " .. edge.from .. " and "
        .. edge.to .. ".")
end

function doPlaceWorker(g,spot,w,k)
  local edge = g.map.edges[spot.edge]
  local stop = edge.stops[spot.stop]
  stop.worker = w
  local thing = stopName(stop.type)
  print(playerColorBB(w.owner) .. " placed a " .. workerName(w.shape) ..
        " on the " .. thing .. " between " .. edge.from .. " and "
        .. edge.to .. ".")
  spawnWorker(w, { stop.x, boardPieceZ, stop.y }, function(o)
    GUI.edge[spot.edge].stops[spot.stop] = o
    k()
  end)
end


function doScorePoints(g,p,n)
  local s = g.playerState[p]
  s.score = s.score + n
  local ui = GUI.player[p]
  ui.score.setValue(s.score .. " VP")

  print(playerColorBB(p) .. " scored " .. n .. " VP.")
end


function doFillOffice(g,n,w,k)
  local node = g.map.nodes[n]
  local x = node.x
  for i,off in ipairs(node.offices) do
    if not off.worker then
      off.worker = w
      spawnWorker(w, {x, boardPieceZ, node.y}, function(o)
        GUI.node[n].offices[i] = o
        print(playerColorBB(w.owner) .. " establised an office in "
                                                          .. node.name)
        if off.vp > 0 then doScorePoints(g,p,off.vp) end
        if i == 1 or node.offices[i - 1].owner.owner ~= p then
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


function doAddExtra(g,n,w,k)
  local node = g.map.nodes[n]
  local x = node.x
  for i,w in ipairs(node.extraOffices) do
    x = x - GUI.officeWidth[w.shape]
  end
  x = x - GUI.officeWidth[w.shape]
  push(node.extraOffices, w)
  spawnWorker(w,{x,boardPieceZ,node.y}, function(o)
    push(GUI.node[n].extraOffices,o)
    print(playerColorBB(w.owner) .. " establised an expansion office in "
                                                          .. node.name)
    k()
  end)
end





function doPlaceBouns(g,p,b,e,k)
  local edge = g.map.edges[e]
  edge.bonus = b
  spawnBonus(b, {edge.x,boardPieceZ,edge.y}, edge.rotation, function(o)
    GUI.edge[e].bonus = o
    print(playerColorBB(p) .. " placed a bonus token between " ..
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
  s.turnReplaceBonus = s.turnReplaceBonus + 1
  spawnPlate(g,p,#s.plates,edge.bonus,function()
    print(playerColorBB(p) .. " picked up the bonus token between " ..
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
  print(playerColorBB(p) .. " used a bonus token")
end


function doUpgradeAction(g,p)
  local s = g.playerState[p]
  if s.actionLevel == #actionLevelMap then return end

  print(playerColorBB(p) .. " upgraded their actions.")
  local curActNum = actionLevelMap[s.actionLevel]
  s.actionLevel = s.actionLevel + 1
  local newActNum = actionLevelMap[s.actionLevel]
  s.turnUsedActions = s.turnUsedActions + (newActNum - curActNum)

  GUI.player[p].actionLevel[s.actionLevel].destroy()
  doChangeActive(g,p,trader,1)
end


function doUpgradeBag(g,p)
  local s = g.playerState[p]
  if s.bagLevel == #bagLevelMap then return end

  print(playerColorBB(p) .. " upgraded their bag.")
  s.bagLevel = s.bagLevel + 1

  GUI.player[p].bagLevel[s.bagLevel].destroy()
  doChangeActive(g,p,trader,1)
end


function doUpgradeBuilding(g,p)
  local s = g.playerState[p]
  if s.buildingLevel == #buildingLevelMap then return end

  print(playerColorBB(p) .. " upgraded their building.")
  s.buildingLevel = s.buildingLevel + 1

  GUI.player[p].buildingLevel[s.buildingLevel].destroy()
  doChangeActive(g,p,trader,1)
end

function doUpgradeKey(g,p)
  local s = g.playerState[p]
  if s.keyLevel == #keyLevelMap then return end

  print(playerColorBB(p) .. " upgraded their keys.")
  s.keyLevel = s.keyLevel + 1

  GUI.player[p].keyLevel[s.keyLevel].destroy()
  doChangeActive(g,p,trader,1)
end

function doUpgradeBook(g,p)
  local s = g.playerState[p]
  if s.bookLevel == #bookLevelMap then return end

  print(playerColorBB(p) .. " upgraded their books.")
  s.bookLevel = s.bookLevel + 1

  GUI.player[p].bookLevel[s.bookLevel].destroy()
  doChangeActive(g,p,merchant,1)
end

function doAddInvest(g,p,i,k)
  local w = { owner = p, shape = merchant }
  g.map.endGameInvest[i] = w
  spawnInvest(g.map, w, i, k)
end






