function workerOptions(game,player,faction)
  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]

  local opts = {}
  local function add(x,f)
    push(opts, { text = x, val = ||f(game,player) })
  end

  if pstate.available > 0 then add("Available worker", useAvailableWorker) end
  if fstate.treasury < 3 then return opts end


  local cost = string.format(" ($%s 3)", faction == arab and "A" or "B")
  local function add(x,f)
    push(opts, { text = x .. cost, val = ||f(game,player,faction) })
  end

  if pstate.casualty  > 0 then add("Hire worker",       useCasualtyWorker)  end
  if fstate.levy      > 0 then add("Reduce levy",       useLevyWorker)      end
  if fstate.mainArmy  > 0 then add("Reduce main army",  useMainArmyWorker)  end
  if fstate.eliteArmy > 0 then add("Reduce elite army", useEliteArmyWorker) end
  if fstate.movement  > 0 then add("Reduce movement",   useMovementWorker)  end

  return opts
end





function checkControlAction(game, opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(player)

  -- What cities can be controlled by the current player
  local choice = {}
  local payments = {}

  for name,city in pairs(game.map.cities) do
    local faction = mayControlCity(city)
    if faction ~= nil then
      local workerOpts = workerOptions(game,player,faction)
      if #workerOpts > 0 then
        push(choice,name)
        payments[name] = workerOpts
      end
    end
  end
  if #choice == 0 then return end

  push(opts,
    { text = "Take Control of a City"
    , val = function()
              askCity(choice, function(city)
                askTextQuick(payments[city], function(makePayment)
                  makePayment()
                  doGainControl(game,player,city)
                end)
              end)
            end
    })
end
