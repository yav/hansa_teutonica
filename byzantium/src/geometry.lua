
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

