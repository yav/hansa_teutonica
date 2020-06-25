
-- Collect free spots from the given edges.
function freeSpotsOn(g,shape,es)
  local opts = {}
  for eid in pairs(es) do
    local has = {}
    local e = g.map.edges[eid]
    for _,s in ipairs(e.stops) do
      if #has == 2 then break end  -- because ther are at most 2 types of conn
      if s.worker == nil and stopAccepts(s,shape) and not has[s.type] then
        push(opts, { x = s.x, y = s.y, r = 0, val = {edge=e.id,stop=s.id} })
        has[s.type] = true
      end
    end
  end
  return opts
end


function freeSpots(g,shape,okRegions)

  local edgeSet = {}
  for _,e in ipairs(g.map.edges) do
    if validRegion(okRegions,e.region) then
      edgeSet[e.id] = true
    end
  end
  return freeSpotsOn(g,shape,edgeSet)
end

-- only consider neigbours in the same region or in the default region
function neighbourEdges(g,es)
  local map = g.map

  local newEs = {}
  for eid,_ in pairs(es) do
    local ed = map.edges[eid]
    for _,nid in ipairs({ed.from, ed.to}) do
      local n = map.nodes[nid]
      for _,e in ipairs(n.edges) do
        local e1 = map.edges[e]
        if (e1.region == ed.region or e1.region == g.map.defaultRegion)
           and not es[e] then newEs[e] = true end
      end
    end
  end
  return newEs
end

-- An adjacent free spot that will accept this shape
function freeAdjacent(g,shape,e)
  local es = {}
  es[e] = true

  local spots = {}
  while #spots == 0 do
    local es = neighbourEdges(g,es)
    es[e] = nil
    spots = freeSpotsOn(g,shape,es)
  end
  return spots
end

function validRegion(okRegs,r)
  if not okRegs then return true end
  for i,_ in pairs(okRegs) do
    if i == r then return true end
  end
  return false
end



function opponentSpots(g,p,onlyTrader,onlyRoad,okRegions)
  local opts = {}

  for _,e in ipairs(g.map.edges) do
    if validRegion(okRegions, e.region) then
      for _,s in ipairs(e.stops) do
        local w = s.worker
        if (s.type == stopRoad or not onlyRoad) and   -- suitable type
            w and                                     -- occupied
            w.owner ~= p and                          -- by someone else
            (w.shape == trader or not onlyTrader)     -- that we can afford
        then
          push(opts, { worker = w, edge = e.id, stop = s.id })
        end
      end
    end
  end

  return opts
end

function occupiedSpots(g,p,r) -- if p == nil, then occupied by anyone
                              -- if r == nil, then any region
  local opts = {}
  for _,e in ipairs(g.map.edges) do
    if not r or e.region == r then
      for _,s in ipairs(e.stops) do
        local w = s.worker
        if w and (not p or w.owner == p) then
          push(opts, { worker = w, edge = e.id, stop = s.id })
        end
      end
    end
  end

  return opts
end

function completedEdges(g,p)
  local xs = {}
  for _,e in ipairs(g.map.edges) do
    local complete = true
    for _,s in ipairs(e.stops) do
      if not s.worker or s.worker.owner ~= p then
        complete = false
        break
      end
    end
    if complete then push(xs,e.id) end
  end
  return xs
end

