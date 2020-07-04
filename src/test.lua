
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

  q.enQ(||doPlaceBonus(g,"Green",bonusAct4,1,q.next))
  q.enQ(||doTakeBonus(g,"Green",1,q.next))

  q.enQ(||nextTurn(g))

end
