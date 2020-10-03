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
  say(string.format("  * The %s army suffered %d casulaties."
                   , playerColorBB(player), todo))

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


function doSingleBattle(game,player,faction,city,opponent,result)
  local cstate = getCity(game,city)
  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]
  local levyBattle  = false
  local siegeBattle = false

  local function armyDice(state)
    local dice = state.mainArmy
    if dice > 3 then dice = 3 end
    dice = dice + state.eliteArmy
    if state.royalty then dice = dice + 1 end
    return dice
  end

  local attackerDice  = 0
  local attackerColor = nil

  local defenderDice  = 0
  local defenderColor = nil
  local dstate        = nil

  if opponent == nil then
    -- Siege
    say(string.format("  * %s besieged %s", playerColorBB(player), city))
    siegeBattle = true
    defenderDice = cstate.strength
    if cstate.fortified then defenderDice = defenderDice + 1 end
    defenderColor = faction_bg_color[cstate.faction]
  else
    attackerDice  = armyDice(fstate)
    attackerColor = playerColor(player)

    defenderColor = getPlayerColor(opponent)
    dstate        = getPlayerState(game,opponent).factions[cstate.faction]
    if dstate.fieldArmy == city then
      -- Against army
      say(string.format( "  * %s attacked the %s army in %s"
                       , playerColorBB(player)
                       , playerColorBB(opponent)
                       , city))

      defenderDice = armyDice(dstate)
    else
      say(string.format( "  * %s attacked the %s levy in %s"
                       , playerColorBB(player)
                       , playerColorBB(opponent)
                       , city))
      -- Against levy
      levyBattle   = true
      defenderDice = dstate.levy
      if defenderDice > 3 then defenderDice = 3 end
    end
  end

  local sem = newSem()
  local attackerHits = 0
  local defenderHits = 0
  if not siegeBattle then
    sem.up()
    rollDice(attackerColor,attacker,attackerDice,
      function(n) attackerHits = n; sem.down() end)
  end

  sem.up()
  rollDice(defenderColor,defender,defenderDice,
      function(n)
        defenderHits = n
        if cstate.constantinople then defenderHits = 2 * defenderHits end
        sem.down()
      end)

  local function checkOutcome()
    local attackerStrength = factionArmySize(fstate) - fstate.movement

    local defenderStrength = 0
    if siegeBattle    then defenderStrength = defenderDice
    elseif levyBattle then defenderStrength = dstate.levy
    else defenderStrength = factionArmySize(dstate) - dstate.movement
    end
    removeDice()
    local outcome = attackerStrength > defenderStrength
    say(string.format("  * %s %s the battle"
                     , playerColorBB(player)
                     , outcome and "won" or "lost"))
    result(outcome)
  end

  sem.wait(|| chooseArmyCasualties(game,player,faction,defenderHits,
              function()
                if siegeBattle then checkOutcome()

                elseif levyBattle then
                  if attackerHits > dstate.levy then
                      attackerHits = dstate.levy
                  end
                  changeLevy(game,player,cstate.faction,-attackerHits)
                  say("The %s levy suffered %d casualties",
                        playerColorBB(opponent), attackerHits)
                  checkOutcome()

                else
                  chooseArmyCasualties(game,opponent,cstate.faction,
                                                attackerHits, checkOutcome)
                end
              end))

end


function doBattle(game,player,fromCityMaybe,city)
  -- ask retreat
  -- check for armies:
  --   if yes, fight them in whatever order
  --      if loose retreat
  --      if win then besige
  --   if no, is there an owner (controller, or emperor in the case of const.)
  --      if no owner, besieg
  --      if yes owner ask if using levy
  --        if no besieg
  --        if yes fight levies
end


function startCivilWar(game,player,city,wopts)
  say(string.format( "\n%s started a civil war in %s"
                   , playerColorBB(player), city ))

  local cstate = getCity(game,city)

  doSingleBattle(game,player,cstate.faction,city,nil,function(res)
    log(res)
    nextTurn(game)
  end)
end

function attackCity(game,player,fromCity,attackedCity)
  log("Attack " .. attackedCity)
  nextTurn(game)
end

function chooseArmyActionFrom(game,player,city,opts)
  local lab = string.format("%s army in %s",playerColorBB(player),city)
  ask(game,player,lab,opts,apply)
end

function chooseArmyAction(game,player,city,movedNo,endActOk)
  local opts = friendlyArmyActions(game,player,city,movedNo,endActOk)
  if opts == nil then
    nextTurn(game)
  else
    chooseArmyActionFrom(game,player,city,opts)
  end
end

function friendlyArmyActions(game,player,city,movedNo,endActOk)

  local hasOpts = false

  local moveOpts = {}
  if movedNo < 2 then
    moveOpts = armyDestinationOptions(game,player,city,movedNo)
    hasOpts = next(moveOpts) ~= nil
  end

  local textOpts = {}
  if endActOk then
    textOpts = { { text = "End Action", val = ||nextTurn(game) } }
    hasOpts  = true
  end

  local civWarActs    = {}
  local cstate        = game.map.cities[city]
  local faction       = cstate.faction
  local spotOpts      = {}
  if faction == byzantium and not cstate.constantinople then
    spotOpts = { byz_civil_war }
  elseif faction == arabs then
    spotOpts = { arab_civil_war_1, arab_civil_war_2 }
  end

  -- XXX: should not be able to start a civil war in our own city
  for _,act in ipairs(spotOpts) do
    if game.actionSpaces[act] == nil then
      local wopts = workerOptions(game,player,faction)
      if next(wopts) ~= nil then
        hasOpts = true
        push(civWarActs, { action = act
                         , val = ||startCivilWar(game,player,city,wopts)
                         })
      end
    end
  end

  if hasOpts then
    return { menu = textOpts, cities = moveOpts, actions = civWarActs }
  else
    return nil
  end

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
      local info = { city     = city
                   , faction  = faction
                   , place    = mayPlaceArmy
                   , armyActs = friendlyArmyActions
                                            (game,player,city,0,mayPlaceArmy)
                   }
      if mayPlaceArmy or hasArmy and info.armyActs ~= nil then
        push(startOpts, { city = city, q = "?", val = info })
      end
    end
  end
  if next(startOpts) == nil then return nil end

  return ||
    ask(game,player,"Choose Start City", { cities = startOpts },
    function(info)
      if info.place then
        doPlaceArmy(game,player,info.faction,info.city,
          ||chooseArmyActionFrom(game,player,info.city,info.armyActs))
      else
        chooseArmyActionFrom(game,player,info.city,info.armyActs)
      end
    end)
end



-- Move an army
function makeMove(game,player,city,moveInfo)
  changeMovement(game,player,moveInfo.faction,-moveInfo.cost)
  if moveInfo.perish then nextTurn(game); return end

  doMoveArmy(game,player,moveInfo.faction,moveInfo.to)
  if moveInfo.attack then
    attackCity(game,player,city,moveInfo.to)
  else
    chooseArmyAction(game,player,moveInfo.to,moveInfo.moveNo,true)
  end
end



--[[
Note on the Byzantine Fleet
===========================

If an activation of the Byzantine fleet makes is so that the player
cannot afford the move then they can pick a different route and there is
no attack on the Arab army.  If there isn't another option, then their action
ends.  See: https://boardgamegeek.com/thread/108294/byzantine-fleet
--]]




-- Try to move an army.  Handles interaction between the byzantium
-- fleet and the arabs.
function tryMoveArmy(game,player,city,moveInfo)

  local byzFleet = game.actionSpaces[byz_fleet]
  if moveInfo.faction == arabs and
     moveInfo.terrain == sea   and
     byzFleet ~= nil           and
     byzFleet ~= player
  then
      -- XXX
    log("Ask Byzantium Fleet Activation")
  else
    makeMove(game,player,city,moveInfo)
  end

end



-- What cities are reachable from the given city for the army of the player.
function armyDestinationOptions(game,player,city,movedNo)
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

    if cost ~= nil and movedNo > 0 then cost = cost + 1 end
    if cost ~= nil and fstate.movement >= cost then
      local perish = cost == factionArmySize(fstate)
      local moveInfo = { to      = target.to
                       , terrain = target.terrain
                       , faction = faction
                       , cost    = cost
                       , perish  = perish
                       , attack  = attack
                       , moveNo  = 1 + movedNo
                       }
      local q = cost
      if perish then q = q .. "☠"
      elseif attack then q = q .. "⚔" end
      push(affordable, { city = target.to, q = q
                       , val  = || tryMoveArmy(game,player,city,moveInfo)
                       })
    end
  end

  return affordable
end
