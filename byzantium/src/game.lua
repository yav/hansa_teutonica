
function newGame(controlledBy)
  local ps = {}
  local i = 1
  for p,_ in pairs(controlledBy) do
    ps[i] = p
    i = i + 1
  end

  shuffle(ps)
  pstates = {}
  for i,p in pairs(ps) do
    pstates[p] = newPlayer(p,i)
  end

  local as = {}

  return
    { version       = versonMaj
    , controlledBy  = controlledBy
    , map           = newMap()
    , finished      = false
    , bulgarArmy    = 7
    , playerState   = pstates
    , actionSpaces  = as
    , nextToPass    = 1   -- to remember in what order people passed
    , endOfRound    = false

    -- player order
    , players     = ps
    , curPlayer   = 1
    , curRound    = 1
    }
end


function playerAfter(game,player)
  local n
  for i,p in ipairs(game.players) do
    if p == player then n = i + 1; break end
  end
  if n > #game.players then n = 1 end
  return game.players[n]
end

function getCurrentPlayer(g)
  return g.players[g.curPlayer]
end

function getPlayerState(g,p)
  return g.playerState[p]
end

function getCity(game,city)
  return game.map.cities[city]
end


--------------------------------------------------------------------------------


function addRoute(map,from,to,terrain)
  push(map.routes, { from = from, to = to, terrain = terrain })
  if terrain == sea then
    local x = map.cities[from]
    if not x.constantinople then x.mediterranean = true end
    x = map.cities[to]
    if not x.constantinople then x.mediterranean = true end
  end
end

function addCity(map,name,faction,strength,x,y)
  local city =
    { strength       = strength
    , controlledBy   = nil
    , fortified      = false
    , faction        = faction
    , bulgarStart    = false
    , constantinople = name == "Constantinople"
    , mediterranean  = false
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
  if city.faction == arabs     then return arabs     end
  return nil
end


--------------------------------------------------------------------------------
function newFaction()
  local vpStats =
    { vpStart = 0
    , vpEnd   = 0
    , vpClaim = 0
    , vpRoyalty = 0
    , vpReligion = 0
    , vpConquer = 0
    , vpMaintenance = 0
    }

  return
    { mainArmy = 0
    , eliteArmy = 0
    , eliteBonus = false  -- emperor/caliph
    , movement = 0
    , levy = 0
    , treasury = 0
    , vp = 0
    , fieldArmy = nil     -- location of field army
    , firstArmyPlacement = true
    , royalty = false
    , religion = 0        -- number of churches/mosks

    -- computed
    , maintenance = 0         -- maintenance at end of round
    , totalCityStrength = 0
    , vpStats = vpStats
    }
end

function factionMaintenance(f)
  local tot = 0
  for stat,cost in pairs(stat_cost) do
    tot = tot + f[stat] * cost
  end
  f.maintenance = tot
end

function factionArmySize(f)
  local size = f.mainArmy + f.eliteArmy + f.movement
  if f.royalty then size = size + 1 end
  return size
end


function newPlayer(color,order)
  local factions = {}

  local a = newFaction()
  a.mainArmy = 8
  a.movement = 5
  a.treasury = 5
  a.vp = 10
  a.vpStats.vpStart = 10
  factionMaintenance(a)
  factions[arabs] = a

  local b = newFaction()
  b.mainArmy = 3
  b.eliteArmy = 1
  b.levy = 2
  b.movement = 2
  b.treasury = 15
  b.vp = 10
  b.vpStats.vpStart = 10
  factionMaintenance(b)
  factions[byzantium] = b

  return
    { casualty = 12
    , available = 9
    , fortifications = 2
    , passed = 0  -- 0 means no; a +ve number indicates in what order we passed
    , taxed = 0   -- how many cubes we have in taxation
    , factions = factions
    , color = color
    , order = order   -- order around table
    }
end



