
function neighbours(game,city,faction)
  local opts = {}

  for _,route in ipairs(game.map.routes) do
    if faction == arabs or
       (faction == bulgars   and route.terrain == road) or
       (faction == byzantium and route.terrain ~= desert)
    then
      if route.from == city then
        push(opts, { to = route.to, terrain = route.terrain })
      elseif route.to == city then
        push(opts, { to = route.from, terrain = route.terrain })
      end
    end
  end

  if faction == byzantium and game.map.cities[city].constantinople then
    for tgt,cstate in pairs(game.map.cities) do
      if cstate.mediterranean then
        push(opts, { to = tgt, terrain = sea })    -- magic route
      end
    end
  end

  return opts
end


function retreatOptions1(game,player,faction,noPerm,city)
  local opts = {}
  local later = {}

  for _,opt in ipairs(neighbours(game,city,faction)) do
    if faction == arabs or opt.terrain ~= desert then
      local otherFaction = game.map.cities[opt.to].faction
      local safe         = otherFaction == faction
      local byzFleet     = game.actionSpaces[byz_fleet]
      local permission   = faction == arabs and opt.terrain == sea and
                           byzFleet ~= nil and byzFleet ~= player

      local tgt = { city        = opt.to
                  , permission  = permission
                  , safe        = safe
                  }
      if permission then
        if not noPerm then push(later, tgt) end
      else
        push(opts,tgt)
      end
    end
  end
  for _,x in ipairs(later) do push(opts,x) end
  return opts
end


function retreatOptionsN(game,player,faction,startCity,noPerm,banned,maxD)
  local visited = {}
  local todo    = { { city     = startCity
                    , distance = 0
                    , ask      = false
                    , safe     = false }
                  }
  local curEl   = 1
  local opts    = {}
  local bestAns = nil

  while (curEl <= #todo) do
    local target = todo[curEl]
    if bestAns ~= nil and target.distance > bestAns then return opts end

    curEl = curEl + 1

    local city  = target.city
    local known = visited[city]
    if not known then
      visited[city] = true
      if target.safe then
        if not banned[city] then
          bestAns    = target.distance
          opts[city] = { cost = target.distance - 1, ask = target.ask }
        end
      elseif target.distance < maxD then
        local dist   = target.distance + 1
        local cities = retreatOptions1(game,player,faction,noPerm,city)
        for _,newTarget in ipairs(cities) do
          push(todo, { city     = newTarget.city
                     , distance = dist
                     , ask      = target.ask or newTarget.permission
                     , safe     = newTarget.safe
                     })
        end
      end
    end
  end

  return opts
end


function bulgarTargets(game)
  local aopts = {}
  local bopts = {}

  local function addCity(city)
    local f = getCity(game,city).faction
    if     f == persians  then aopts[city] = true; bopts[city] = true
    elseif f == arabs     then                     bopts[city] = true;
    elseif f == byzantium then aopts[city] = true;
    end
  end

  for city,cstate in pairs(game.map.cities) do
    if cstate.faction == bulgars then
      for _,tgt in ipairs(neighbours(game,city,bulgars)) do
        addCity(tgt.to)
      end
    else
      if cstate.bulgarStart then addCity(city) end
    end
  end

  return aopts,bopts
end


function improveTargets(game)
  local player = getCurrentPlayer(game)
  local opts = {}
  opts[byzantium] = {}
  opts[arabs] = {}
  for city,cstate in pairs(game.map.cities) do
    if cstate.controlledBy == player and cstate.strength < 3 then
      push(opts[cstate.faction], city)
    end
  end
  return opts
end

function fortifyTargets(game)
  local player = getCurrentPlayer(game)
  local opts = {}
  opts[byzantium] = {}
  opts[arabs] = {}
  for city,cstate in pairs(game.map.cities) do
    if cstate.controlledBy == player and not cstate.fortified then
      push(opts[cstate.faction], city)
    end
  end
  return opts
end

