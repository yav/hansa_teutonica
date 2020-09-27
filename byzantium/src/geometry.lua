
-- for movement
function neighbours(game,city)
  local opts = {}

  for _,route in ipairs(game.map.routes) do
    if route.from == city then
      push(opts, { to = route.to, terrain = route.terrain })
    elseif route.to == city then
      push(opts, { to = route.from, terrain = route.terrain })
    end
  end

  if game.map.cities[city].constantinople then
    for tgt,cstate in pairs(game.map.cities) do
      if cstate.mediterranean then
        push(opts, { to = tgt, terrain = sea })
      end
    end
  end

  return opts
end

function retreatOptions1(game,player,faction,visited,city)
  local optsOk      = {}
  local optsMore    = {}
  local done = false
  for _,opt in ipairs(neighbours(game,city)) do
    if not visited[opt.to] and (faction == arabs or opt.terrain ~= desert) then
      visited[opt.to] = true
      local otherFaction = game.map.cities[opt.to].faction
      local safe      = otherFaction == faction
      local byzFleet  = game.actionSpaces[byz_fleet]
      local dangerous = faction == arabs and
                        opt.terrain == sea and
                        byzFleet ~= nil and
                        byzFleet ~= player
      local cost = 0
      if not safe then cost = 1 end
      local tgt = { city = opt.to
                  , dangerous = dangerous
                  , cost = cost
                  }

      if safe then
        push(optsOk,tgt)
        if not dangerous then done = true end
      else
        push(optsMore, tgt)
      end
    end
  end
  return done,optsOk,optsMore
end

function retreatOptions(game,player,faction,visited,city)
  local done,optsOk,optsMore =
          retreatOptions1(game,player,faction,visited,curCity)
  if done then return optsOk end

  for _,opt in ipairs(optsMore) do
    for _,res in ipairs(retreatOptions(game,player,faction,visited,opt.city)) do
      res.cost = rest.cost + 1
      res.dangerous = res.dangerous or opt.dangerous
      push(optsOk,res)
    end
  end
  return optsOk
end

