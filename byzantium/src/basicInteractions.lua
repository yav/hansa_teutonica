-- | Compute what cubes are available to spend on actions for the given faction.
function workerOptions(game,player,faction)
  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]
  local cost   = "$3 " .. faction_currency[faction]
  local pcost  = "? (" .. cost .. ")"

  local opts = {}

  if pstate.available > 0 then
    opts.available =
      { q = "?"
      , val = function()
                changeAvailableWorkers(game,player,-1)
                say(playerColorBB(player) .. " used an available worker:")
              end
      }
  end

  if fstate.treasury < 3 then return opts end

  if pstate.casualty > 0 then
    opts.casualty =
      { q   = pcost
      , val = function()
                changeTreasury(game,player,faction,-3)
                changeCasualties(game,player,-1)
                local msg = string.format("%s hired a casualty:\n  * for %s"
                                         , playerColorBB(player),cost)
                say(msg)
              end
      }
  end

  local check = { eliteArmy = changeEliteArmy
                , mainArmy  = changeMainArmy
                , levy      = changeLevy
                , movement  = changeMovement
                }

  for fName,fState in pairs(pstate.factions) do
    local os = {}
    local have = false
    for stat,how in pairs(check) do
      if fState[stat] > 0 then
        have = true
        os[stat] =
          { q   = pcost
          , val = function ()
                    changeTreasury(game,player,faction,-3)
                    local msg = string.format("%s spent a worker:\n  * from %s %s\n  * for %s"
                              , playerColorBB(player)
                              , faction_poss[fName]
                              , faction_stat_name[stat]
                              , cost
                              )
                    how(game,player,fName,-1)
                    say(msg)
                  end
         }
      end
    end
    if have then opts[fName] = os end
  end

  return opts
end


function chooseArmyCasualties(game,player,faction,todo,k)
  if player == "bulgars" or faction == bulgars then
    say(string.format("  * The %s army suffered %d casulaties"
                   , faction_poss[bulgars], todo))
    changeBulgars(game,-todo)
    k()
    return
  end

  say(string.format("  * The %s army suffered %d casulaties"
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
          { q = "?"
          , val = function()
                    changeFactionStat(stat)(game,player,faction,-1)
                    changeCasualties(game,player,1)
                    done = done + 1
                    doOne()
                  end
          }
      end
    end
    if next(opts) == nil then
      if fstate.royalty then changeRoyalty(game,player,faction,false) end
      k()
    else
      local lab = string.format("Choose casualty %d/%d",done,todo)
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

  local function doRetreat(tgtCity,cost)
    if tgtCity == nil then
      doDestroyArmy(game,player,faction)
      k()
    else
      doMoveArmy(game,player,faction,tgtCity)
      local msg = string.format("  * The %s %s army retreated to %s"
                               , playerColorBB(player)
                               , faction_poss[faction]
                               , city)
      say(msg)
      chooseArmyCasualties(game,player,faction,cost,k)
    end
  end

  local function askPermission(city,cost)
    local byzFleet = game.actionSpaces[byz_fleet]
    local opts = { { text = "Yes", val = || doRetreat(city,cost) }
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
                                        else doRetreat(city,info.cost)
                            end
                          end
                 })
    end

    if next(qopts) ~= nil then
      local lab = "Choose retreat city"
      ask(game,player,lab, { cities = qopts },apply)
    else
      doRetreat(nil,nil)
    end
  end

  interact()
end

-- faction  == bulgars                    => attack with bulgars
-- faction  == byzantium/arams            => attack with player's army
-- opponent == nil                        => siege
-- opponent == "bulgars"                  => opponnet is bulgar army
-- opponent == "Red"  and player has army => Red's is army
-- opponent == "Red"  and no army         => Red's levy
function doSingleBattle(game,player,faction,city,opponent,result)
  local cstate = getCity(game,city)
  local fstate = nil
  local who = nil
  if faction == bulgars then
    who             = "The " .. faction_poss[bulgars] .. " army"
    fstate          = newFaction()
    fstate.mainArmy = game.bulgarArmy
  else
    who          = "The " .. playerColorBB(player) .. " army"
    local pstate = getPlayerState(game,player)
    fstate       = pstate.factions[faction]
  end

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
  local whoDefends    = nil

  if opponent == nil then
    -- Siege
    say(string.format("  * %s besieged %s", who, city))
    siegeBattle = true
    defenderDice = cstate.strength
    if cstate.fortified then defenderDice = defenderDice + 1 end
    defenderColor = faction_bg_color[cstate.faction]
  else
    attackerDice  = armyDice(fstate)
    attackerColor = faction == bulgars and faction_bg_color[bulgars]
                    or playerColor(player)

    if opponent == "bulgars" then
      whoDefends       = "the " .. faction_poss[bulgars] .. " army"
      defenderColor    = faction_bg_color[bulgars]
      dstate           = newFaction()
      dstate.mainArmy  = game.bulgarArmy
      dstate.fieldArmy = city
    else
      whoDefends    = "the " .. playerColorBB(opponent) .. " army"
      defenderColor = getPlayerColor(opponent)
      dstate        = getPlayerState(game,opponent).factions[cstate.faction]
    end
    if dstate.fieldArmy == city then
      -- Against army

      defenderDice = armyDice(dstate)
    else
      -- Against levy
      whoDefends = "the " .. playerColorBB(opponent) .. " levy"
      levyBattle   = true
      defenderDice = dstate.levy
      if defenderDice > 3 then defenderDice = 3 end
    end
    say(string.format( "  * %s attacked %s in %s", who, whoDefends, city))
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
    if faction == bulgars then attackerStrength = game.bulgarArmy end

    local defenderStrength = 0
    if siegeBattle    then defenderStrength = defenderDice
    elseif levyBattle then defenderStrength = dstate.levy
    elseif opponent == "bulgars" then defenderStrength = game.bulgarArmy
    else defenderStrength = factionArmySize(dstate) - dstate.movement
    end
    removeDice()
    local outcome = attackerStrength > defenderStrength
    say(string.format("  * %s %s the battle"
                     , who
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
                        whoDefends, attackerHits)
                  checkOutcome()

                else
                  chooseArmyCasualties(game,opponent,cstate.faction,
                                                attackerHits, checkOutcome)
                end
              end))

end


--[[
"player" is the player initiating the attack
"faction" is the faction that will benefit if successful
"city" is where the battle is happening
"usingBulgars" indicates if the player is using the bulgars to attck
"ifLost" is a callback to call if the player looses the battle

if "faction" matches the faction of the city then we have a civial war
--]]
function doWar(game,player,faction,city,usingBulgars,ifLost)
  local cstate = getCity(game,city)
  local attacker = faction
  if usingBulgars then attacker = bulgars end
  local isCivilWar = attacker == cstate.faction

  local q = actQ()

  -- Check for retreat
  local opponents = {}
  if cstate.faction == byzantium or cstate.faction == arabs then
    local p = player
    local count = 0
    while count < #game.players do
      local pstate = getPlayerState(game,p)
      local fstate = pstate.factions[cstate.faction]
      if fstate.fieldArmy == city then
        if p == player then
          if not isCivilWar then
            q.enQ(||chooseRetreat(game,player,cstate.faction,city,q.next))
          end
        else
          local menu =
            { { text = "Defend City"
              , val  = function() anyArmies = push(opponents,p); q.next() end
              }
            , { text = "Retreat"
              , val  = chooseRetreat(game,player,cstate.faction,city,q.next)
              }
            }
          q.enQ(||ask(game,player,"Choose Army Action", { menu = menu }, apply))
        end
      end
      p = playerAfter(game,p)
      count = count + 1
    end
  end

  -- Check for levy
  q.enQ(function()
    if next(opponents) ~= nil then q.next(); return end
    local defender = cstate.controlledBy
    if defender == nil then
      if cstate.constantinople then defender = game.actionSpaces[emperor] end
      -- Not sure if it matters if they lost the emperor cube.
      -- Let's say it doesn't matter.
    end
    if defender == nil then
      if cstate.faction == bulgars then opponents = { "bulgars" } end
      q.next()
      return
    end

    local pstate = getPlayerState(game,defender)
    local fstate = pstate.factions[cstate.faction]
    if fstate.levy <= 0 then q.next(); return end

    local menu =
      { { text = "Defend with Levy"
        , val  = function() push(opponents,defender); q.next() end
        }
      , { text = "Do not Defend"
        , val  = q.next
        }
     }
    ask(game,defender,"Use Levy?",{menu=menu},apply)
  end)

  local function doBattles()
    local opts = {}
    for _,p in pairs(opponents) do
      push(opts, { text = "Fight " .. (p == "bulgars" and faction_name[bulgars]
                                       or playerColorBB(p))
                 , val  = function()
                    doSingleBattle(game,player,attacker,city,p,function(won)
                      if won then doBattles() else ifLost(game) end
                    end)
                   end
                 })
    end

    local n = #opts
    if n == 0 then q.next(); return end
    if n == 1 then opts[1].val(); return end
    ask(game,players,"Choose Opponent",{menu=opts},apply)
  end

  -- Do battles
  q.enQ(doBattles)

  -- Do siege
  q.enQ(function()
    doSingleBattle(game,player,attacker,city,nil,function(won)
      if won
      then conquerCity(game,player,city,faction,usingBulgars)
      else ifLost(game)
      end
    end)
  end)

end


function startCivilWar(game,player,city,act,wopts)
  local quest = "Choose civial war worker"
  local cstate = getCity(game,city)
  local q = actQ()

  q.enQ(||ask(game,player,quest,{cubes=wopts},function(f)
    f()
    markAction(game,player,act)
    say(string.format( "  * %s started a civil war in %s"
                     , playerColorBB(player), city ))
    q.next()
  end))

  q.enQ(||doWar(game,player,cstate.faction,city,false,nextTurn))
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

For simplicty we just implement it to mean "attacker goes back".
Reading through the BGG forums it seems that the designer only talks
about retreting in the context of defending armyies although there is
no official ruling one way or the other.
--]]
function attackCity(game,player,fromCity,attackedCity)
  local faction = getCity(game,fromCity).faction

  doWar(game,player,faction,attackedCity,false,
  function()
    if getPlayerState(game,player).factions[faction].fieldArmy ~= nil then
      doMoveArmy(game,player,faction,fromCity)  -- or retreat??
    end
    nextTurn(game)
  end)
end

function chooseArmyActionFrom(game,player,city,opts)
  local lab = "Action for army in " .. city
  ask(game,player,lab,opts,apply)
end

function chooseArmyAction(game,player,city,movedNo,endActOk)
  say(lab)
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

  if cstate.controlledBy ~= player then
    local faction       = cstate.faction
    local spotOpts      = {}
    if faction == byzantium and not cstate.constantinople then
      spotOpts = { byz_civil_war }
    elseif faction == arabs then
      spotOpts = { arab_civil_war_1, arab_civil_war_2 }
    end

    for _,act in ipairs(spotOpts) do
      if game.actionSpaces[act] == nil then
        local wopts = workerOptions(game,player,faction)
        if next(wopts) ~= nil then
          hasOpts = true
          push(civWarActs, { action = act
                           , val = ||startCivilWar(game,player,city,act,wopts)
                           })
        end
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

  return function()
    say(string.format("\n%s is using an army", playerColorBB(player)))
    ask(game,player,"Choose army",{ cities = startOpts },
    function(info)
      if info.place then
        doPlaceArmy(game,player,info.faction,info.city,
          ||chooseArmyActionFrom(game,player,info.city,info.armyActs))
      else
        chooseArmyActionFrom(game,player,info.city,info.armyActs)
      end
    end)
  end
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




function conquerCity(game,player,city,faction,usingBulgars)
  local cstate = getCity(game,city)
  local owner = cstate.controlledBy
  if owner ~= nil then
    if cstate.fortified then
      cstate.fortified = false
      changeFortifications(game,owner,1)
    else
      changeCasualties(game,owner,1)
    end
  end
  cstate.controlledBy = nil

  local function doGaintControl()
    if not usingBulgars then
      cstate.controlledBy = player
      say(string.format("  * %s conquered %s",playerColorBB(player),city))
    else
      say(string.format("  * The %s conquered %s",faction_name[bulgars],city))
    end

    local newStrength = cstate.strength - 1
    if not usingBulgars then
      changeTreasury(game,player,faction,newStrength)
      say(string.format("  * Gained %d %s", newStrength,
                                            faction_currency[faction]))
    end
    changeVP(game,player,faction,newStrength)
    say(string.format("  * Gained %d %s VP", newStrength
                                           , faction_poss[faction]))

    if newStrength == 0 then newStrength = 1 end
    cstate.strength = newStrength
    cstate.faction  = usingBulgars and bulgars or faction

    -- XXX: check for fall of Constantinople
    redrawCity(game,city,||nextTurn(game))
  end

  if usingBulgars then
    doGaintControl()
  else
    local wopts = workerOptions(game,player,faction)
    if next(wopts) == nil then
      chooseArmyCasualties(game,player,faction,2,function()
        changeCasualties(game,player,-1)
        doGaintControl()
      end)
    else
      local lab = "Choose worker to govern"
      ask(game,player,lab, { cubes = wopts }, function(f)
        f()
        doGaintControl()
      end)
    end
  end
end



