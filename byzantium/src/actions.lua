
function nextTurn(game)
  takeTurn(game)
end

function takeTurn(game)
  local opts = {}
  checkControlAction(game,opts)
  checkIncreaseArmy(game,opts)
  checkMove(game,opts)
  checkTest(game,opts)
  local player = getCurrentPlayer(game)
  local quest = string.format("%s's turn:",playerColorBB(player))
  ask(game,player,quest,{menu = opts},apply)
end


function checkTest(game,opts)
  local player = getCurrentPlayer(game)

  local function test()

    local sem = newSem()
    local aVal = nil
    local dVal = nil

    sem.up(); rollDice(playerColor(player),attacker,8,
                        function(n) aVal = n sem.down() end)
    sem.up(); rollDice(faction_bg_color[byzantium],defender,4,
                        function(n) dVal = n sem.down() end)
    sem.wait(function()
      log(aVal)
      log(dVal)
      Wait.frames(removeDice,120)
      nextTurn(game)
    end)

  end

  push(opts,{ text = "Test", val = test })
end


function checkControlAction(game, opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(game,player)

  -- What cities can be controlled by the current player
  local choice = {}
  local payments = {}
  local fpayments = {}
  for faction,_ in pairs(pstate.factions) do
    fpayments[faction] = workerOptions(game, player, faction)
  end

  for name,city in pairs(game.map.cities) do
    local faction = mayControlCity(city)
    if faction ~= nil then
      local p = fpayments[faction]
      if next(p) ~= nil then
        push(choice,{ city = name, q = "?", val = name })
        payments[name] = p
      end
    end
  end
  if #choice == 0 then return end

  push(opts,
    { text = "Claim a City"
    , val = |    | ask(game,player,"Claim which city?", {cities=choice},
            |city| ask(game,player,"Choose Worker",{cubes=payments[city]},
            function(f)
              f()
              doGainControl(game,player,city,||nextTurn(game))
            end
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

  local function actIncrease()
    local needSep = true
    local aopts = {}
    for faction,_ in pairs(pstate.factions) do
      local fopts = {}
      local wopts = workerOptions(game,player,faction)
      if next(wopts) ~= nil then
        for _,stat in ipairs({ "eliteArmy", "mainArmy", "levy", "movement" }) do
          if stat ~= "eliteArmy" or not placedElite then
            fopts[stat] = 
               { q = "?", val =
                 function()
                   ask(game,player,"Choose worker to reassign",{cubes=wopts},
                   function(pay)
                     pay()
                     first = false
                     placedElite = stat == "eliteArmy"
                     changeFactionStat(stat)(game,player,faction,1)
                     local msg = string.format("  * %s %s increased"
                                              , faction_poss[faction]
                                              , faction_stat_name[stat])
                     say(msg)
                     actIncrease()
                   end)
                end
              }
          end
        end
      end
      if next(fopts) ~= nil then
        aopts[faction] = fopts
      end
    end
    local menu = {}
    if not first then
      menu = { { text = "Done", val = ||nextTurn(game) } }
    end
    local lab = string.format("%s army to increase",playerColorBB(player))
    ask(game,player,lab,{menu=menu,cubes=aopts},apply)
  end

  push(opts,
    { text = "Increase Army"
    , val  = actIncrease
    })
end




function performAttack(game,player,fromCity,warCity)
  log("XXX: attack")
  nextTurn(game)
end

--[[
Note on Retreating
==================

There is some ambiguity in the rules with exactly what happens when an
attacker looses a battle.  The rules state that they must "retreat" to
the city they came from, however, it is not clear if full retreat rules
apply, or if it simply means the attacker just goes back to where they were.

This matters if an arab army attacks via a sea route and looses,
as with "retreat rules" the byzanteed fleet could interfere thus destroying
them.
--]]



function checkMove(game,opts)

  local pickArmy = chooseArmyToMove(game,getCurrentPlayer(game))

  if pickArmy == nil then
    return opts
  else
    push(opts, { text = "Use Army", val = pickArmy })
  end
end


