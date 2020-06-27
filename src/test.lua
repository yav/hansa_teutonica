
function where()
  local p = Player["White"].getPointerPosition()
  local msg = string.format("road(%.2f, %.2f)", p[1], p[3])
  log(msg)
end

function loc(r)
  local p = Player["White"].getPointerPosition()
  local msg = string.format(",%.2f, %.2f,%d)", p[1], p[3], r)
  log(msg)
end

function seeEdge(g,e)
  local ed = g.map.edges[e]
  log(ed.from .. " -> " .. ed.to)
end


function test(g)

  local q = actQ()
  q.enQ(||doPlaceWorker(g,{edge=1,stop=1},{owner="Green",shape=trader},q.next))
  q.enQ(||doPlaceWorker(g,{edge=1,stop=2},{owner="Green",shape=trader},q.next))
  q.enQ(||doPlaceWorker(g,{edge=1,stop=3},{owner="Green",shape=trader},q.next))
  q.enQ(||doPlaceBouns(g,"Green",bonusAct4,1,q.next))
  q.enQ(function () startTurn(g,"Green"); q.next() end)
  q.enQ(||doFillOffice(g,"Cardiff",{owner="Green",shape=trader},q.next))

  q.enQ(function ()doUpgradeAction(g,"Green"); q.next() end)
  q.enQ(function ()doUpgradeBag(g,"Green"); q.next() end)
  q.enQ(function ()doUpgradeBuilding(g,"Green"); q.next() end)
  q.enQ(function ()doUpgradeBook(g,"Green"); q.next() end)
  q.enQ(function ()doUpgradeKey(g,"Green"); q.next() end)
  q.enQ(||nextTurn(g))

  -- q.enQ(||doTakeBonus(g,"Green",1,q.next))
  -- q.enQ(||doUseUpBonus(g,"Green",1,q.next))

end
