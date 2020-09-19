
function newGame(ps)
  shuffle(ps)

  pstates = {}
  for _,p in ipairs(ps) do
    pstates[p] = newPlayer()
  end

  return
    { map         = newMap()
    , finished    = false
    , bulgarArmy  = 7
    , playerState = pstates

    -- player order
    , players     = ps
    , curPlayer   = 1
    , curRound    = 1
    }
end

function getNextPlayer(g)
  local p = g.curPlayer + 1
  if p > #g.players then p = 1 end
  return p
end

function getCurrentPlayer(g)
  return g.players[g.curPlayer]
end

function getPlayerState(g,p)
  return g.playerState[p]
end

--------------------------------------------------------------------------------


function addRoute(map,from,to,terrain)
  push(map.routes, { from = from, to = to, terrain = terrain })
end

function addCity(map,name,faction,strength,x,y)
  local city =
    { strength       = strength
    , controlledBy   = nil
    , fortified      = false
    , faction        = faction
    , bulgarStart    = false
    , constantinople = name == "Constantinople"
    , x              = x
    , y              = y
    }
  map.cities[name] = city
  return city
end

-- returns the faction that can ontrol this city, or nil
function mayControlCity(city)
  if city.controlledBy ~= nil or city.constantinople then return nil end
  if city.faction == byzantium then return byzantium end
  if city.faction == arab      then return arab      end
  return nil
end


--------------------------------------------------------------------------------
function newFaction()
  return
    { mainArmy = 0
    , eliteArmy = 0
    , eliteBonus = false  -- emperor/caliph
    , movement = 0
    , levy = 0
    , treasury = 0
    , vp = 0
    , fieldArmy = nil     -- location of field army
    }
end


function newPlayer(color,order)
  local factions = {}

  local a = newFaction()
  a.mainArmy = 8
  a.movement = 5
  a.treasury = 5
  a.vp = 10
  factions[arabs] = a

  local b = newFaction()
  b.mainArmy = 3
  b.eliteArmy = 1
  b.levy = 2
  b.movement = 2
  b.treasury = 15
  b.vp = 10
  factions[byzantium] = b

  return
    { casualty = 12
    , available = 9
    , fortifications = 2
    , passed = 0  -- 0 means no; a +ve number indicates in what order we passed
    , factions = factions
    , color = color
    , order = order   -- order around table
    }
end



