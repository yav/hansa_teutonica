-- | Compute what cubes are available to spend on actions for the given faction.
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


function chooseArmyCasualties(game,player,faction,todo,k)
  if todo == 0 then k(); return end

  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]
  local done = 1
  if factionArmySize(fstate) <= todo then
    doDestroyArmy(game,player,faction)
    k()
    return
  end

  local function doOne()
    if done > todo then k(); return end

    local opts = {}
    for _,stat in ipairs({"movement","mainArmy","eliteArmy"}) do
      if fstate[stat] > 0 then
        opts[stat] =
          function()
            changeFactionStat(stat)(game,player,faction,-1)
            changeCasualties(game,player,1)
            done = done + 1
            doOne()
          end
      end
    end
    if next(opts) == nil then
      if fstate.royalty then changeRoyalty(game,player,faction,false) end
      k()
    else
      local lab = string.format("%s casualty %d/%d",
                                        playerColorBB(player),done,todo)
      local cubeOpts = {}
      cubeOpts[faction] = opts
      ask(game,player,lab,{cubes=cubeOpts},apply)
    end
  end

  doOne()
end


function chooseRetreat(game,player,faction,city,k)
  local pstate = getPlayerState(game,player)
  local limit  = factionArmySize(pstate.factions[faction])
  local banned = {}
  local alwaysNo = false
  local interact

  local function askPermission(city,cost)
    local byzFleet = game.actionSpaces[byz_fleet]
    local opts = { { text = "Yes", val = ||k(city,cost) }
                 , { text = "No"
                   ,  val = function() banned[city] = true; interact() end
                   }
                 , { text = "Disallow any sea retreat"
                   , val = function() alwaysNo = true; interact() end
                   }
                 }

    ask(game,byzFleet,"Allow sea retreat to " .. city .. "?", {menu=opts}
       , apply)
  end

  interact = function()
    local opts = retreatOptionsN(game,player,faction,city,alwaysNo,banned,limit)

    local qopts = {}
    for city,info in pairs(opts) do
      local lab = info.cost
      if info.ask then lab = lab .. '!' end
      push(qopts, { city = city
                  , q = lab
                  , val = function()
                            if info.ask then askPermission(city,info.cost)
                                        else k(city,info.cost)
                            end
                          end
                 })
    end

    if next(qopts) ~= nil then
      ask(game,player,"Choose city to retreat to:", { cities = qopts },apply)
    else
      k(nil,nil)
    end
  end

  interact()
end



function chooseArmyToMove(game,player)
  local pstate = getPlayerState(game,player)

  local startOpts = {}
  for city,cstate in pairs(game.map.cities) do
    local faction = cstate.faction
    if faction == byzantium or faction == arabs then
      local fstate  = pstate.factions[faction]
      local hasArmy = fstate.fieldArmy == city
      local mayPlaceArmy = fstate.fieldArmy == nil and
                           factionArmySize(fstate) > 0 and
                           (faction == arabs or not fstate.firstArmyPlacement)
      local info = { city = city
                   , moveOpts = armyDestinationOptions(game,player,city,false)
                   }
      if mayPlaceArmy then
        push(startOpts, { city = city, q = "?", val = info })
      elseif hasArmy and next(info.moveOpts) ~= nil then
        push(startOpts, { city = city, q = "?", val = info })
      end
    end
  end
  if next(startOpts) == nil then return nil end

  return |k|
    ask(game,player,"Choose Start City", { cities = startOpts }, function(info)
      local cstate = game.map.cities[info.city]
      local armyFaction = cstate.faction
      if pstate.factions[armyFaction].fieldArmy == nil then
        doPlaceArmy(game,player,armyFaction,info.city,||k(info))
      else
        k(info)
      end
    end)
end


function armyDestinationOptions(game,player,city,sndMove)
  local cstate  = game.map.cities[city]
  local faction = cstate.faction
  local pstate  = getPlayerState(game,player)
  local fstate  = pstate.factions[faction]
  local targets = neighbours(game,city,faction)

  local affordable = {}
  for _,target in ipairs(targets) do
    local cost = nil    -- means infinite
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

    if cost ~= nil and sndMove then cost = cost + 1 end
    if cost ~= nil and fstate.movement >= cost then
      local perish = cost == factionArmySize(fstate)
      local moveInfo = { to      = target.to
                       , terrain = target.terrain
                       , faction = faction
                       , cost    = cost
                       , perish  = perish
                       , attack  = attack
                       }
      local q = cost
      if perish then q = q .. "☠"
      elseif attack then q = q .. "⚔" end
      push(affordable, { city = target.to, q = q, val = moveInfo })
    end
  end

  return affordable
end
