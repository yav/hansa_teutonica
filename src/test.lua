
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

  local function addOffice(p,n,k)
    startTurn(g,p)
    doFillOffice(g,n,{owner=p,shape=trader},k)
  end

  q.enQ(||doPlaceWorker(g,{ edge = 26, stop = 1},{owner="Green",shape=trader},q.next))
  q.enQ(||doPlaceWorker(g,{ edge = 26, stop = 2},{owner="Green",shape=trader},q.next))
  q.enQ(||doPlaceWorker(g,{ edge = 1, stop = 1},{owner="Purple",shape=trader},q.next))
  q.enQ(||doPlaceWorker(g,{ edge = 1, stop = 2},{owner="Purple",shape=trader},q.next))
  q.enQ(||doPlaceBonus(g,"Green",bonusUpgrade,1,q.next))
  q.enQ(||doTakeBonus(g,"Green",1,q.next))
  q.enQ(||doPlaceBonus(g,"Green",bonusSwap,1,q.next))
  q.enQ(||doTakeBonus(g,"Green",1,q.next))
  q.enQ(||doPlaceBonus(g,"Green",bonusMove,1,q.next))
  q.enQ(||doTakeBonus(g,"Green",1,q.next))
  q.enQ(||doPlaceBonus(g,"Green",bonusExtra,1,q.next))
  q.enQ(||doTakeBonus(g,"Green",1,q.next))
  q.enQ(||doPlaceBonus(g,"Green",bonusAct3,1,q.next))
  q.enQ(||doTakeBonus(g,"Green",1,q.next))
  q.enQ(||doPlaceBonus(g,"Green",bonusAct4,1,q.next))
  q.enQ(||doTakeBonus(g,"Green",1,q.next))
  q.enQ(||addOffice("Green","London",q.next))
  q.enQ(||addOffice("Purple","London",q.next))
  q.enQ(||nextTurn(g))


end
