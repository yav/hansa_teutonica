
function where()
  local p = Player["Green"].getPointerPosition()
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

  local function addOffice(p,n,t,k)
    startTurn(g,p)
    doFillOffice(g,n,{owner=p,shape=t},k)
  end

--[[
  for n,node in pairs(g.map.nodes) do
    for _,o in ipairs(node.offices) do
      q.enQ(||addOffice("Green",n,o.shape,q.next))
    end
  end

  for e,edge in ipairs(g.map.edges) do
    q.enQ(||doPlaceBonus(g,"Green",bonusAct4,e,q.next))
    for i,s in ipairs(edge.stops) do
      local sh = (s.type == stopShip) and merchant or trader
      q.enQ(||doPlaceWorker(g,{ edge=e,stop=i},{ owner = "Yellow",shape = sh},q.next))
    end
  end
  q.enQ(||doAddExtraRight(g,"Dresden",{owner="Green",shape=trader},q.next))
  q.enQ(||doAddExtraRight(g,"Dresden",{owner="Yellow",shape=trader},q.next))
  q.enQ(||doAddExtraRight(g,"Dresden",{owner="Purple",shape=trader},q.next))
  q.enQ(||doAddExtra(g,"Dresden",{owner="Red",shape=trader},q.next))
--]]
  q.enQ(||nextTurn(g))

end
