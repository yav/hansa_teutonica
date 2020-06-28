
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
  local e = 7
  q.enQ(function() doUpgradeBuilding(g,"Green"); q.next() end)

  q.enQ(||doPlaceBouns(g,"Green",bonusSwap,e,q.next))
  q.enQ(||doTakeBonus(g,"Green",e,q.next))
  q.enQ(function () startTurn(g,"Green"); q.next() end)
  q.enQ(||doFillOffice(g,"London",{ owner = "Green", shape = trader },q.next))

  q.enQ(function () startTurn(g,"Purple"); q.next() end)
  q.enQ(||doFillOffice(g,"London",{ owner = "Purple", shape = trader },q.next))




  q.enQ(||nextTurn(g))


end
