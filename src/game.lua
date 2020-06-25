function newGame(colors,mkMap) -- Game
  local game = {}
  local m = newMap()
  mkMap(m)
  game.map = m
  game.players = colors
  game.playerState = {}
  game.curPlayer = 0
  game.turn = 0

  for turnOrder,color in ipairs(colors) do
    game.playerState[color] = newPlayer(color,turnOrder)
  end

  return game
end


function newMap()        -- Map
  local map     = {}
  map.nodes     = {}
  map.edges     = {}
  map.regions   = {}
  map.defaultRegion = nil   -- can always place things here
  return map
end



function newNode(map,name,regions,x,y)    -- Node
  local node    = {}
  node.name     = name
  node.region   = regions
  node.x        = boardDX + x
  node.y        = boardDY + y
  node.edges    = {}
  node.offices  = {}
  node.extraOffices = {}  -- first one is closest to the city
  node.gateway  = {}
  node.action   = nil

  map.nodes[name] = node
  return node
end

function getNode(map,name) -- Node
  return map.nodes[name]
end

-- For access to special regions
function getRightMost(g,n)
  local node = g.map.nodes[n]
  local x
  for _,off in ipairs(node.offices) do
    if not off.worker then break end
    x = off.worker
  end
  return x
end

function getController(g,n)
  local nd = g.map.nodes[n]

  local num = {}
  local rightMost = {}
  local best = 0
  local control

  local function addW(pos,w)
    local p = w.owner
    local n = num[p]
    if not n then n = 0; rightMost[p] = pos end
    n = n + 1
    num[p] = n
    if n > best or (n == best and rightMost[p] > rightMost[control]) then
      control = p
      best = n
    end
  end

  -- first one is right most
  for i,w in ipairs(nd.extraOffices) do
    addW(-i,w)
  end

  for i,off in ipairs(nd.offices) do
    local w = off.worker
    if not w then break end
    addW(i,w)
  end

  return control
end

function nextFreeOffice(g,n)
  local node = g.map.nodes[n]
  for _,off in ipairs(node.offices) do
    if not off.worker then return off end
  end
  return nil
end


function addOffice(node, shape, level)        -- Office
  local office = {}
  office.vp    = 0
  office.shape = shape
  office.level = level
  office.worker = nil

  office.id    = push(node.offices, office)
  return office
end

function addGateway(node,region)    -- void
  push(node.gateway, region)
end


function newEdge(map,node1,node2,region,x,y,r)    -- Edge
  local edge  = {}
  edge.x      = x + boardDX
  edge.y      = y + boardDY
  edge.rotation = r
  edge.region = region
  edge.from   = node1.name
  edge.to     = node2.name
  edge.stops  = {}
  edge.bonus  = nil
  edge.id     = push(map.edges,edge)

  push(node1.edges,edge.id)
  push(node2.edges,edge.id)
  return edge
end

function addStop(edge, type, x, y)  -- Stop
  local stop    = {}
  stop.type     = type
  stop.x        = x + boardDX
  stop.y        = y + boardDY
  stop.edge     = edge.id
  stop.worker   = nil
  stop.id       = push(edge.stops,stop)
  return stop
end

function stopAccepts(stop, shape)
  return stop.type == stopRoad or
         stop.type == stopShip and shape == merchant
end



function newPlayer(color,turnOrder)  -- Player
  local p = {}
  p.color = color
  p.turnOrder = turnOrder

  p.actionLevel = 1
  p.bagLevel = 1
  p.bookLevel = 1
  p.buildingLevel = 1
  p.keyLevel = 1

  p.finishedPlates = 0
  p.plates = {}

  p.active = {}
  p.active[trader] = 4 + turnOrder
  p.active[merchant] = 1

  p.passive = {}
  p.passive[trader] = 7 - turnOrder
  p.passive[merchant] = 0

  p.turnActions = 0
  p.turnUsedActions = 0
  p.turnReplaceBonus = 0

  p.score = 0
  return p
end



