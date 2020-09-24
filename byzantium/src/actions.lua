function workerOptions(game,player,faction,k)
  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]
  local cost   = faction == arabs and " $A 3" or " $B 3"

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


function nextTurn(game)
  takeTurn(game)
end

function takeTurn(game)
  local opts = {}
  checkControlAction(game,opts)
  checkIncreaseArmy(game,opts)
  local player = getCurrentPlayer(game)
  local quest = string.format("%s's turn:",playerColorBB(player))
  askText(game,player,quest,opts,|f|f())
end



function checkControlAction(game, opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(game,player)

  -- What cities can be controlled by the current player
  local choice = {}
  local payments = {}

  for name,city in pairs(game.map.cities) do
    local faction = mayControlCity(city)
    if faction ~= nil then
      local workerOpts = workerOptions(game,player,faction
                                      , || doGainControl(game,player,name
                                      , || nextTurn(game)))
      if #workerOpts > 0 then
        push(choice,name)
        payments[name] = workerOpts
      end
    end
  end
  if #choice == 0 then return end

  push(opts,
    { text = "Claim a City"
    , val = |    | askCity(game,player,"Claim which city?",choice,
            |city| askTextQuick(game,player,"Choose worker:",payments[city],
            |   f| f()
            ))
    })
end



function checkIncreaseArmy(game, opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(game,player)

  if pstate.available == 0 and
     pstate.factions[byzantium].treasury < 3 and
     pstate.factions[arabs].treasury < 3 then return
  end

  local placedElite = false
  local first = true

  local function incStat(faction,stat,payment)
    first = false
    placedElite = stat == "eliteArmy"
    changeFactionStat(stat)(game,player,faction,1)
    askTextQuick(game,player,"Reassign to " .. faction_stat_name[stat],payment,
                      |f|f())
  end

  local function actIncrease()
    local needSep = true
    local aopts = {}
    for faction,_ in pairs(pstate.factions) do
      if needSep then push(aopts, { text = nil }); needSep = false end
      local wopts = workerOptions(game,player,faction,actIncrease)
      if #wopts ~= 0 then
        for _,stat in ipairs({ "eliteArmy", "mainArmy", "levy", "movement" }) do
          if stat ~= "eliteArmy" or not placedElite then
            needSep = true
            push(aopts,
              { text = faction_name[faction] .. " " .. faction_stat_name[stat]
              , val  = || incStat(faction,stat,wopts)
              })
          end
        end
      end
    end
    if not first then
      if needSep then push(aopts, { text = nil }); needSep = false end
      push(aopts, { text = "Done", val = ||nextTurn(game) })
    end
    askText(game,player,"Army to increase:",aopts,|f|f())
  end

  push(opts,
    { text = "Increase Army"
    , val  = actIncrease
    })

end


