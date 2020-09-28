
function neighbours(game,city,faction)
  local opts = {}

  for _,route in ipairs(game.map.routes) do
    if faction == arabs or route.terrain ~= desert then
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

function retreatOptions1(game,player,faction,city)
  local opts = {}
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
      push(opts, tgt)
    end
  end
  return opts
end


function retreatOptionsN(game,player,faction,startCity,banned,startN)
  local visited = {}
  local opts    = {}

  local function search(curCity,ask,cost,n)
    local cities = retreatOptions1(game,player,faction,curCity)

    -- Look for safe neighbours
    if n == 1 then
      for _,tgt in ipairs(cities) do
        local nextCity = tgt.city
        if tgt.safe and not visited[nextCity] and not banned[nextCity] then
          local known = opts[nextCity]
          local newAsk = ask or tgt.permission
          if known == nil or cost < known.cost then
            opts[nextCity] = { ask = newAsk, cost = cost }
          elseif cost == known.cost then
            known.ask = known.ask and newAsk
          end
        end
      end

    else
      -- Look for unsafe neighbours or safe neighbours that require permission
      for _,tgt in ipairs(cities) do
        local nextCity = tgt.city
        if not visited[nextCity] and
           (not tgt.safe or tgt.safe and tgt.permission) then
          visited[nextCity] = true
          local newCost = cost
          if not tgt.safe then newCost = newCost + 1 end
          search(nextCity,ask or tgt.permission,newCost,n-1)
          visited[nextCity] = nil
        end
      end
    end
  end
  search(startCity,false,0,startN)
  return opts
end


