function newGame(colors,mkMap) -- Game
  local game = {}
  local m = newMap()
  mkMap(m)
  game.map = m
  game.players = colors
  game.playerState = {}
  game.curPlayer = 0
  game.turn = 0

  game.raceAward = 1

  game.endGame = false

  game.bonus = nil
  game.nextBonus = 0
  setupBonusTokens(game)

  for turnOrder,color in ipairs(colors) do
    game.playerState[color] = newPlayer(color,turnOrder)
  end

  return game
end

function setupBonusTokens(g)
  local startingTokens = { bonusSwap, bonusExtra, bonusMove }
  shuffle(startingTokens)
  local ix = 1
  for _,edge in ipairs(g.map.edges) do
    if edge.startingBonus then
      edge.bonus = startingTokens[ix]
      ix = ix + 1
    end
  end

  local toks = {}
  ix = 1
  for i,n in ipairs(bonusNum) do
    if n == bonusSwap or n == bonusExtra or n == bonusMove then n = n - 1 end
    for j = 1,n do
      toks[ix] = i
      ix = ix + 1
    end
  end
  shuffle(toks)
  g.bonus = toks
  g.nextBonus = 1
end


function newMap()        -- Map
  local map     = {}
  map.nodes     = {}
  map.edges     = {}
  map.regions   = {}
  map.defaultRegion = nil   -- can always place things here

  map.investX       = 0
  map.investY       = 0
  map.endGameInvest = {}
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


function nodeHasFree(g,n)
  local node = g.map.nodes[n]
  for _,o in ipairs(node.offices) do
    if not o.worker then return true end
  end
  return false
end

function hasPresence(g,p,n)
  local node = g.map.nodes[n]
  for _,o in ipairs(node.offices) do
    if not o.worker then break end
    if o.worker.owner == p then return true end
  end

  for _,w in ipairs(node.extraOffices) do
    if w.owner == p then return true end
  end
  return false

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

function addAction(node,act)
  node.action = act
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
  edge.startingBonus = false
  edge.bonus  = nil
  edge.id     = push(map.edges,edge)

  push(node1.edges,edge.id)
  push(node2.edges,edge.id)
  return edge
end

function edgeAcceptsBonus(g,edge)
  if edge.bonus then return false end

  if not nodeHasFree(g,edge.from) and
     not nodeHasFree(g,edge.to) then return false end

  for _,stop in ipairs(edge.stops) do
    if stop.worker then return false end
  end

  return true
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
  p.turnReplaceBonus = {}

  p.foreignBuilds = 0
  p.foreignBuildsIn = {}

  p.score = 0
  return p
end


function gainForeignBuilds(g,p,node)
  if #node.gateway == 0 then return end
  local s = g.playerState[p]
  local b = s.foreignBuilds
  s.foreignBuilds = b + 1
  for _,r in ipairs(node.gateway) do
    local b = s.foreignBuildsIn[r]
    s.foreignBuildsIn[r] = b + 1
  end
end

function startTurn(g,p)
  local s = g.playerState[p]
  s.turnActions = actionLevelMap[s.actionLevel]
  s.turnUsedActions = 0
  s.turnReplaceBonus = {}

  local foreignBuildsIn = {}
  for _,r in ipairs(g.map.regions) do
    if r ~= g.map.defaultRegion then foreignBuildsIn[r] = 0 end
  end
  s.foreignBuilds = 0
  s.foreignBuildsIn = foreignBuildsIn

  for n,node in pairs(g.map.nodes) do
    local w = getRightMost(g,n)
    if w and p == w.owner then gainForeignBuilds(g,p,node) end
  end
end

-- returns the set of foreign regions in which we can build
function accessibleRegions(g,p)
  local regs = {}
  regs[g.map.defaultRegion] = true
  local s = g.playerState[p]
  if s.foreignBuilds == 0 then return regs end

  for r,n in pairs(s.foreignBuildsIn) do
    if n > 0 then regs[r] = true end
  end

  return regs
end

-- update foreignBuilds given that we just built on e
function noteBuiltOn(g,p,e)
  local edge = g.map.edges[e]
  local r = edge.region
  if r == g.map.defaultRegion then return end
  local s = g.playerState[p]
  s.foreignBuilds = s.foreignBuilds - 1
  local b = s.foreignBuildsIn[r]
  s.foreignBuildsIn[r] = b - 1
end

-- Returns the index of the given bonus if we have it, or nil.
function haveBounusToken(g,p,b)
  local s = g.playerState[p]
  for i,tok in ipairs(s.plates) do
    if tok == b then return i end
  end
  return nil
end

