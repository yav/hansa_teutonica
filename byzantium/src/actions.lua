function workerOptions(game,player,faction,k)
  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]
  local cost   = faction == arab and " $A 3" or " $B 3"

  local opts = {}

  if pstate.available > 0 then
    push(opts, { text = "Available worker"
               , val  = function()
                          changeAvailableWorkers(game,player,-1)
                          say(playerColorBB(player) ..
                                          " used an available worker.")
                          k()
                        end
               })
  end

  if fstate.treasury < 3 then return opts end

  if pstate.casualty > 0 then
    push(opts, { text = "Hire worker" .. cost
               , val = function()
                         changeTreasury(game,player,faction,-3)
                         changeCasualties(game,player,-1)
                         say(playerColorBB(player) ..
                            " used a casualty for" .. cost)
                         k()
                       end
               })
  end

  local armyOpts = {}
  local check = { eliteArmy = { "Elite army", changeEliteArmy }
                , mainArmy  = { "Main army",  changeMainArmy }
                , levy      = { "Levy",       changeLevy }
                , movement  = { "Movement",   changeMovement }
                }

  for fName,fState in pairs(pstate.factions) do
    local os = {}
    for stat,how in pairs(check) do
      if fState[stat] > 0 then
        push(os, { text = how[1]
                 , val  = function ()
                            changeTreasury(game,player,faction,-3)
                            how[2](game,player,fName,-1)
                            say(playerColorBB(player) ..
                                  " used a worker from the " ..
                                  faction_name[fName] .. " " .. how[1] ..
                                  " for" .. cost)
                            k()
                          end
                 })
      end
    end
    if #os > 0 then
      local nm = faction_name[fName]
      push(armyOpts, { text = nm
                     , val  = { quest = string.format("Reduce %s:",nm)
                              , opts  = os
                              }
                     })
    end
  end

  if #armyOpts > 0 then
    push(opts, { text = "Reduce army" .. cost
               , val  = | | askTextQuick(game,player,"Which army?",armyOpts,
                        |o| askTextQuick(game,player,o.quest,o.opts,
                        |f| f())
                        )
               })
  end

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
