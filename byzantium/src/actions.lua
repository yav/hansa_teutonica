function workerOptions(game,player,faction)
  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]
  local cost   = faction == arabs and " $A 3" or " $B 3"

  local opts = {}

  if pstate.available > 0 then
    opts.available =
      function()
        changeAvailableWorkers(game,player,-1)
        say(playerColorBB(player) .. " used an available worker.")
      end
  end

  if fstate.treasury < 3 then return opts end

  if pstate.casualty > 0 then
    opts.casualty =
      function()
        changeTreasury(game,player,faction,-3)
        changeCasualties(game,player,-1)
        say(playerColorBB(player) ..  " used a casualty for" .. cost)
      end
  end

  local check = { eliteArmy = { "Elite army", changeEliteArmy }
                , mainArmy  = { "Main army",  changeMainArmy }
                , levy      = { "Levy",       changeLevy }
                , movement  = { "Movement",   changeMovement }
                }

  for fName,fState in pairs(pstate.factions) do
    local os = {}
    local have = false
    for stat,how in pairs(check) do
      if fState[stat] > 0 then
        have = true
        os[stat] =
           function ()
             changeTreasury(game,player,faction,-3)
             how[2](game,player,fName,-1)
             say(playerColorBB(player) ..
               " used a worker from the " ..  faction_name[fName] ..
               " " .. how[1] ..  " for" .. cost)
           end
      end
    end
    if have then opts[fName] = os end
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
  checkMove(game,opts)
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
  local fpayments = {}
  for faction,_ in pairs(pstate.factions) do
    fpayments[faction] = workerOptions(game, player, faction)
  end

  for name,city in pairs(game.map.cities) do
    local faction = mayControlCity(city)
    if faction ~= nil then
      local p = fpayments[faction]
      if next(p) ~= nil then
        push(choice,{ city = name, q = "?" })
        payments[name] = p
      end
    end
  end
  if #choice == 0 then return end

  push(opts,
    { text = "Claim a City"
    , val = |    | askCity(game,player,"Claim which city?",choice,{},
            |city| askCube(game,player,"Choose Worker",payments[city],
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
            fopts[stat] = function()
              askCube(game,player,"Choose worker to reassign",wopts,
              function(pay)
                pay()
                first = false
                placedElite = stat == "eliteArmy"
                changeFactionStat(stat)(game,player,faction,1)
                say(playerColorBB(player) .. " increased " ..
                      faction_name[faction] .. " " ..  faction_stat_name[stat])
                actIncrease()
              end)
            end
          end
        end
      end
      if next(fopts) ~= nil then
        aopts[faction] = fopts
      end
    end
    if not first then
      aopts.text = { { text = "Done", val = ||nextTurn(game) } }
    end
    askCube(game,player,"Army to increase:",aopts,|f|f())
  end

  push(opts,
    { text = "Increase Army"
    , val  = actIncrease
    })
end




function performAttack(game,player,fromCity,warCityt)
  log("XXX: attack")
  nextTurn(game)
end


function checkMove(game,opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(game,player)
  local moveNumber = 1

--[[
Note on Byzantine Fleet:
If an activation of the Byzantine fleet makes is so that the player
cannot afford the move then they can pick a different route and there is
no attack on the Arab army.  If there isn't another option, then their action
ends.  See: https://boardgamegeek.com/thread/108294/byzantine-fleet
--]]

  local startOpts = {}
  for city,cstate in pairs(game.map.cities) do
    local faction = cstate.faction
    if faction == byzantium or faction == arabs then
      local fstate  = pstate.factions[faction]
      local hasArmy = fstate.fieldArmy == city
      local mayPlaceArmy = fstate.fieldArmy == nil and
                           (faction == arabs or not fstate.firstArmyPlacement)
      if hasArmy or mayPlaceArmy then
        push(startOpts, { city = city, q = "?" })
      end
    end
  end
  if next(startOpts) == nil then return opts end

  -- Perform the selected move
  local function makeMove(fromCity,moveInfo)
    changeMovement(game,player,moveInfo.faction,-moveInfo.cost)
    doMoveArmy(game,player,moveInfo.faction,moveInfo.to)
    moveNumber = moveNumber + 1
    if moveInfo.attack then
      performAttack(game,player,fromCity,moveInfo.to)
    else
      -- XXX: ask for civil war or more movement
      local nextOpts = {}
      nextTurn(game)
    end

  end

  -- Try to move from one city to another.  This works unless the
  -- Byzantine fleet interferese, which could cancel the move
  -- (because it cannot be afforder, or army got destroyed) or change its cost.
  local function tryMove(fromCity,moveInfo)
    local byzFleet = game.actionSpaces[byz_fleet]
    if moveInfo.faction == arabs and
       moveInfo.terrain == sea   and
       byzFleet ~= nil           and
       byzFleet ~= player then
      -- XXX
    else makeMove(fromCity,moveInfo)
    end
  end

  -- Choose where to move from the given city
  local function askWhere(city)
    local cstate  = game.map.cities[city]
    local faction = cstate.faction
    local fstate  = pstate.factions[faction]
    local targets = neighbours(game,city)
    local affordable = {}
    for _,target in ipairs(targets) do
      local cost = nil    -- means infinit
      local tgtS = game.map.cities[target.to]
      local terrain = target.terrain
      if     terrain == road then cost = 1
      elseif terrain == sea  then
        if   faction == byzantium then cost = 1
        else
          local hasArabFleet = game.actionSpaces[arab_fleet] == player
          cost = hasArabFleet and 1 or 2
          if tgtS.constantinople then cost = cost * 2 end
        end
      elseif terrain == desert then
        cost = (faction == arabs) and 1 or nil
      end
      local attack = tgtS.faction ~= faction
      if tgtS.controlledBy == player and attack then cost = nil end

      if cost ~= nil and moveNumber > 1 then cost = cost + 1 end
      if cost ~= nil and fstate.movement >= cost then
        local moveInfo = { to      = target.to
                         , terrain = target.terrain
                         , faction = faction
                         , cost    = cost
                         , attack  = attack
                         }
        local q = cost
        if attack then q = q .. "⚔" end
        push(affordable, { city = target.to, q = q, val = moveInfo })
      end
    end

    local endAct = { text = "End Action", val = nil }
    askCity(game,player,"Move to which city?",affordable,{ endAct },
      function(tgt)
        if tgt == nil then nextTurn(game); return end
        tryMove(city,tgt)
      end)


  end

  local function pickArmy()
    askCity(game,player,"Choose Start City", startOpts, {}, function(city)
      local cstate = game.map.cities[city]
      local armyFaction = cstate.faction
      if pstate.factions[armyFaction].fieldArmy == nil then
        doPlaceArmy(game,player,armyFaction,city,||askWhere(city,false))
      else
        askWhere(city,false)
      end
    end)
  end

  push(opts, { text = "Move Army" , val = pickArmy })
end


