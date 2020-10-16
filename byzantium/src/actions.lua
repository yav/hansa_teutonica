

function endGame(game,normalEnd)

  disableUndo()
  undo_state = nil

  local controlledCities = {}
  for _,p in ipairs(game.players) do
    controlledCities[p] = 0
  end

  for city,cstate in pairs(game.map.cities) do
    local player = cstate.controlledBy
    if player ~= nil then
      if normalEnd then
        changeVP(game,player,cstate.faction,cstate.strength)
      end
      if normalEnd or cstate.faction == arabs then
        local n = controlledCities[player]
        controlledCities[player] = n + 1
      end
    end
  end

  local score      = {}
  local totalVP    = {}
  local totalMoney = {}
  for player,pstate in pairs(game.playerState) do
    local a   = pstate.factions[arabs]
    local b   = pstate.factions[byzantium]
    local aVP     = a.vp
    local aMoney  = a.treasury
    local bVP     = b.vp
    local bMoney  = b.treasury
    if not normalEnd then
      bVP = 0
      bMoney = 0
    end

    local bigVP   = math.max(aVP,bVP)
    local smallVP = math.min(aVP,bVP)
    if 2 * smallVP < bigVP then smallVP = 0 end
    score[player]      = bigVP + smallVP
    totalVP[player]    = aVP + bVP
    totalMoney[player] = aMoney + bMoney
  end

  local function largest(stat)
    local ps = {}
    local m  = 0
    for p,v in pairs(stat) do
      if v > m        then ps = { p }; m = v
      elseif v == m   then push(ps,p)
      end
    end
    return ps
  end

  local pt = 0.5
  for _,p in ipairs(largest(totalVP)) do
    local s = score[p]
    score[p] = s + pt
  end

  pt = pt / 2
  for _,p in ipairs(largest(controlledCities)) do
    local s = score[p]
    score[p] = s + pt
  end

  pt = pt / 2
  for _,p in ipairs(largest(totalMoney)) do
    local s = score[p]
    score[p] = s + pt
  end

  local finalScore = {}
  for p,s in pairs(score) do
    push(finalScore, { player = p, score = s })
  end

  table.sort(finalScore, |x,y| x.score > y.score)
  spawnMenu(25,12,function(menu)
    spawnMenuItem(nil,menu,0,"Final Score",nil)
    for i,info in pairs(finalScore) do
      local lab = string.format("%s %.3f VP", playerColorBB(info.player)
                                            , info.score)
      spawnMenuItem(nil,menu,i,lab,nil)
    end
  end)

end

function endRound(game)
  disableUndo()
  undo_state = nil

  -- income
  for city,cstate in pairs(game.map.cities) do
    local owner = cstate.controlledBy
    if owner ~= nil then
      changeTreasury(game,owner,cstate.faction,2 * cstate.strength)
    end
  end

  -- maintenance
  local q = actQ()
  for _,player in ipairs(game.players) do
    local pstate = getPlayerState(game,player)
    for _,faction in ipairs({arabs,byzantium}) do
      local fstate = pstate.factions[faction]
      local have   = fstate.treasury
      local need   = 0
      for _,stat in ipairs({"eliteArmy","mainArmy","movement","levy"}) do
        need = need + fstate[stat] * stat_cost[stat]
      end
      if have >= need then
        changeTreasury(game,player,faction,-need)
      else
        -- we just do these in a somewhat arbitrary order
        q.enQ(||maintenanceWithPenalty(game,player,faction,need,q.next))
      end
    end
  end

  q.enQ(function()

    -- Return action cubes
    for act,owner in pairs(game.actionSpaces) do
      changeAvailableWorkers(game,owner,1)
      updateActionOwner(act,nil)
    end
    game.actionSpaces = {}

    -- Retrun from taxes, pass, and casualty/2
    for player,pstate in pairs(game.playerState) do
      local n = pstate.taxed
      changeTaxes(game,player,-n)
      changeAvailableWorkers(game,player,n)
      changeRoyalty(game,player,arabs,false)
      changeRoyalty(game,player,byzantium,false)
      local p = pstate.passed
      if p == 1 then game.curPlayer = pstate.order end
      resetPass(game,player)

      local hires = math.ceil(pstate.casualty / 2)
      changeCasualties(game,player,-hires)
      changeAvailableWorkers(game,player,hires)
    end


    game.nextToPass = 1
    game.endOfRound = false

    local r = game.curRound
    if r == 3 then endGame(game,true); return end
    game.curRound = r + 1
    moveRoundMarker(game,k)
    takeTurn(game)
  end)
end

function nextTurn(game)
  local pnum = #game.players
  local nextPass = game.nextToPass

  if     game.endOfRound         then endRound(game); return
  elseif game.nextToPass == pnum then game.endOfRound = true
  end

  local n = game.curPlayer
  local pstate
  repeat
    n = n + 1
    if n > pnum then n = 1 end
    pstate = getPlayerState(game,game.players[n])
  until pstate.passed == 0
  game.curPlayer = n

  takeTurn(game)
end

function takeTurn(game)
  local save = doSave(game)
  local opts = {}
  checkControlAction(game,opts)
  checkIncreaseArmy(game,opts)
  checkMove(game,opts)
  checkBulgars(game,opts)
  checkRoyalty(game,opts)
  checkFleet(game,opts)
  checkImproveCity(game,opts)
  checkFortifyCity(game,opts)
  checkTaxes(game,opts)
  checkTemple(game,opts)
  checkPass(game,opts)
  local player = getCurrentPlayer(game)
  local quest = string.format("%s's turn:",playerColorBB(player))
  ask(game,player,quest,opts,function(f)
    pushUndo(player,save)
    apply(f)
  end)
end

function addTextOption(opts,x)
  local menu = opts.menu
  if menu == nil then menu = {}; opts.menu = menu end
  push(menu,x)
end

function addActOption(opts,x)
  local acts = opts.actions
  if acts == nil then acts = {}; opts.actions = acts end
  push(acts,x)
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

  addTextOption(opts,
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

  local num = 1

  local function actIncrease()
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
                     placedElite = stat == "eliteArmy"
                     changeFactionStat(stat)(game,player,faction,1)
                     num = num + 1
                     local msg = string.format("  * %s %s increased"
                                              , faction_poss[faction]
                                              , faction_stat_name[stat])
                     say(msg)
                     if num <= 3 then actIncrease() else nextTurn(game) end
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
    menu = { { text = "Done", val = ||nextTurn(game) } }
    local lab = string.format("Increase army %d/3",num)
    ask(game,player,lab,{menu=menu,cubes=aopts},apply)
  end

  addTextOption(opts,
    { text = "Increase Army"
    , val  = actIncrease
    })
end




function checkMove(game,opts)

  local pickArmy = chooseArmyToMove(game,getCurrentPlayer(game))

  if pickArmy == nil then
    return opts
  else
    addTextOption(opts, { text = "Use Army", val = pickArmy })
  end
end


function checkBulgars(game,opts)
  local player      = getCurrentPlayer(game)
  local aworker     = workerOptions(game,player,arabs)
  local bworker     = workerOptions(game,player,byzantium)
  local atgts,btgts = bulgarTargets(game)
  local bulgarArmy  = game.bulgarArmy

  local benefactor = {}
  for _,ben in ipairs({ {arabs,aworker,atgts}, {byzantium,bworker,btgts} }) do
    local faction = ben[1]
    local workers = ben[2]
    local targets = ben[3]
    if next(workers) ~= nil and (next(targets) ~= nil or bulgarArmy <= 7) then
      push(benefactor, { text = "Use " .. faction_name[faction]
                       , val  = { faction = faction
                                , workers = workers
                                , targets = targets
                                }
                       })
    end
  end

  if next(benefactor) == nil then return opts end

  local function use(act)
    ask(game,player,"Choose Sponsor",{menu=benefactor},function(info)
      ask(game,player,"Choose Diplomat",{cubes=info.workers},function(f)
        f()
        local msg = string.format("  * to use %s on behalf of %s"
                                 , faction_name[bulgars]
                                 , faction_name[info.faction])
        say(msg)
        markAction(game,player,act)
        changeBulgars(game,2)
        local cities = {}
        for city,_ in pairs(info.targets) do
          push(cities, { city = city, q = "⚔", val = city })
        end
        local menu = {}
        if game.bulgarArmy <= 9 then
          push(menu, { text = "Bulgar Army +2", val = nil })
        end
        ask(game,player, "Bulgar Army Action",
                        { menu = menu, cities = cities }, function(city)
          if city == nil then
            changeBulgars(game,2)
            nextTurn(game)
          else
            doWar(game,player,info.faction,city,true,nextTurn)
          end
        end)
      end)
    end)
  end

  for _,act in ipairs({bulgars_1,bulgars_2}) do
    if game.actionSpaces[act] == nil then
      addActOption(opts, { action = act, val = ||use(act) })
    end
  end
end


function checkRoyalty(game, opts)
  for _,faction in ipairs({byzantium,arabs}) do
    local action = emperor
    if faction == arabs then action = caliph end
    if game.actionSpaces[action] == nil then
      local player = getCurrentPlayer(game)
      local wopts  = workerOptions(game,player,faction)
      if next(wopts) ~= nil then
        addActOption(opts,
          { action = action
          , val = function()
              ask(game, player, "Choose worker", { cubes = wopts },
              function(pay)
                pay()
                markAction(game,player,action)
                changeRoyalty(game,player,faction,true)
                changeVP(game,player,faction,2)
                local name = faction == arabs and "Caliph" or "Emperor"
                say("  * to influence the " .. name)
                say(string.format("  * +2 %s VP", faction_poss[faction]))
                nextTurn(game)
              end)
            end
          })
      end
    end
  end
end

function checkFleet(game, opts)
  for _,faction in ipairs({byzantium,arabs}) do
    local action = byz_fleet
    if faction == arabs then action = arab_fleet end
    if game.actionSpaces[action] == nil then
      local player = getCurrentPlayer(game)
      local wopts  = workerOptions(game,player,faction)
      if next(wopts) ~= nil then
        addActOption(opts,
          { action = action
          , val = function()
              ask(game, player, "Choose worker", { cubes = wopts },
              function(pay)
                pay()
                markAction(game,player,action)
                say(string.format("  * to claim the %s fleet",
                                                    faction_poss[faction]))
                nextTurn(game)
              end)
            end
          })
      end
    end
  end
end


function checkImproveCity(game, opts)
  local actions = {}
  actions[arabs]     = { arab_improve_1, arab_improve_2, arab_improve_3 }
  actions[byzantium] = { byz_improve_1, byz_improve_2 }

  local hasSpot = false
  for _,as in pairs(actions) do
    for _,a in ipairs(as) do
      if game.actionSpaces[a] == nil then hasSpot = true; break end
    end
    if hasSpot then break end
  end
  if not hasSpot then return end

  local cityOpts = {}
  local player   = getCurrentPlayer(game)
  for faction,cities in pairs(improveTargets(game)) do
    local qs = {}
    cityOpts[faction] = qs
    local wopts = workerOptions(game,player,faction)
    if next(wopts) ~= nil then
      for _,city in ipairs(cities) do
        push(qs, {city = city, q = "+1", val = { wopts = wopts, city = city }})
      end
    end
  end
  if next(cityOpts) == nil then return end

  for faction,actions in pairs(actions) do
    local copts = cityOpts[faction]
    if next(copts) ~= nil then
      for _,action in ipairs(actions) do
        if game.actionSpaces[action] == nil then
          addActOption(opts,
            { action = action
            , val =
              ||     ask(game,player,"City to Improve", { cities = copts },
              |info| ask(game, player, "Choose worker", { cubes = info.wopts },
              function(pay)
                pay()
                markAction(game,player,action)
                local city = info.city
                local cstate = getCity(game,city)
                cstate.strength = cstate.strength + 1
                redrawCity(game,city,function()
                  say("  * to improve " .. city)
                  nextTurn(game)
                end)
              end))
            })
        end
      end
    end
  end

end



function checkFortifyCity(game, opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(game,player)
  if pstate.fortifications == 0 then return end

  local actions = { fortify_1, fortify_2 }

  -- Don't bother computing other stuff if there are no actions left.
  local hasSpot = false
  for _,action in ipairs(actions) do
    if game.actionSpaces[action] == nil then hasSpot = true; break end
  end
  if not hasSpot then return end

  local cityOpts = {}
  for faction,cities in pairs(fortifyTargets(game)) do
    if next(cities) ~= nil then
      local wopts = workerOptions(game,player,faction)
      if next(wopts) ~= nil then
        for _,city in ipairs(cities) do
          push(cityOpts, { city = city, q = "+1"
                         , val = { wopts = wopts, city = city }
                         })
        end
      end
    end
  end
  if next(cityOpts) == nil then return end

  for _,action in ipairs(actions) do
    if game.actionSpaces[action] == nil then
      addActOption(opts,
        { action = action
        , val = function()
            ask(game,player,"City to Fortify",{cities=cityOpts},function(info)
              ask(game,player,"Choose Worker",{cubes=info.wopts},function(pay)
                pay()
                markAction(game,player,action)
                changeFortifications(game,player,-1)
                local city = info.city
                getCity(game,city).fortified = true
                redrawCity(game,city,function()
                  say("  * to fortify " .. city)
                  nextTurn(game)
                end)
              end)
            end)
          end
        })
    end
  end

end

function checkTaxes(game,opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(game,player)
  if pstate.taxed > 0 or pstate.available == 0 then return end

  local choices = { { text = "$2 " .. faction_currency[byzantium]
                    , val  = {2,0}
                    }
                  , { text = "$2 " .. faction_currency[arabs]
                    , val  = {0,2}
                    }
                  , { text = "$1 " .. faction_currency[byzantium]
                          .. " and $1 " .. faction_currency[arabs]
                    , val = {1,1}
                    }
                  , { text = "Done"
                    , val = nil
                    }
                  }

  local function doTaxes()
    if pstate.available == 0 then nextTurn(game); return end

    ask(game,player,"Spend available worker?",{menu=choices},function(delta)
      if delta == nil then nextTurn(game); return end
      changeTreasury(game,player,byzantium,delta[1])
      changeTreasury(game,player,arabs,delta[2])
      changeAvailableWorkers(game,player,-1)
      changeTaxes(game,player,1)
      doTaxes()
    end)
  end

  addActOption(opts, { action = taxes, val = doTaxes })
end


function checkPass(game,opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(game,player)
  if pstate.passed > 0 then return end

  addActOption(opts,
    { action = pass
    , val = function()
        if pstate.casualty > 0 then
          changeCasualties(game,player,-1)
          setPass(game,player,||nextTurn(game))
        else
          local opts = {}
          local function using(f)
            return { q = "?"
                   , val = function()
                             f()
                             changePass(game,player,true,||nextTurn(game))
                           end
                   }
          end
          if pstate.available > 0 then
            opts.available = using(||changeAvailableWorkers(game,player,-1))
          end
          for _,faction in ipairs({arabs,byzantium}) do
            local fstat = pstate.factions[faction]
            local ents = {}
            opts[faction] = ents
            for _,stat in ipairs({"eliteArmy","mainArmy","movement","levy"}) do
              if fstat[stat] > 0 then
                ents[stat] =
                   using(||changeFactionStat(stat)(game,player,faction,-1))
              end
            end
          end
          ask(game,player,"Worker to pass with",{cubes=opts},apply)
        end
      end
    })
end


function checkTemple(game,opts)
  local player  = getCurrentPlayer(game)
  local pstate  = getPlayerState(game,player)
  for _,faction in ipairs({arabs,byzantium}) do
    local fstate = pstate.factions[faction]
    local treasury = fstate.treasury
    if treasury >= 6 then
      fstate.treasury = treasury - 6
      wopts = workerOptions(game,player,faction)
      fstate.treasury = treasury
      if next(wopts) ~= nil then
        local act = church
        if faction == arabs then act = mosk end
        addActOption(opts,
          { action = act
          , val = function()
                    ask(game,player,"Choose Worker",{cubes=wopts},function(pay)
                      pay()
                      changeTreasury(game,player,faction,-6)
                      changeVP(game,player,faction,2)
                      changeReligion(game,player,faction,1)
                      say("  * to build a " .. faction_temple[faction])
                      nextTurn(game)
                    end)
                  end
          })
      end
    end
  end

end
