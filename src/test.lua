
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

  q.enQ(||doPlaceBouns(g,"Green",bonusAct3,e,q.next))
  q.enQ(||doTakeBonus(g,"Green",e,q.next))
  q.enQ(||doPlaceBouns(g,"Green",bonusAct4,e,q.next))
  q.enQ(||doTakeBonus(g,"Green",e,q.next))
  q.enQ(||doPlaceBouns(g,"Green",bonusExtra,e,q.next))
  q.enQ(||doTakeBonus(g,"Green",e,q.next))
  q.enQ(||doPlaceBouns(g,"Green",bonusMove,e,q.next))
  q.enQ(||doTakeBonus(g,"Green",e,q.next))
  q.enQ(function()
    startTurn(g,"Purple")
    doFillOffice(g,"Cambridge",{owner="Purple",shape=merchant},q.next)
  end)
  q.enQ(||doPlaceWorker(g,{edge=1,stop=1},{owner="Purple",shape=trader},q.next))
  q.enQ(||doPlaceWorker(g,{edge=1,stop=2},{owner="Green",shape=trader},q.next))
  q.enQ(||doPlaceWorker(g,{edge=1,stop=3},{owner="Green",shape=merchant},q.next))
  q.enQ(||nextTurn(g))


end
