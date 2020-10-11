
--------------------------------------------------------------------------------
-- Actions

function markAction(game,player,action)
  game.actionSpaces[action] = player
  GUI.actions[action].setColorTint(playerColor(player))
end

function unmarkAction(g,action)
  g.actions[action] = nil
  GUI.actions[action].setColorTint(Color(0,0,0,0))
end


--------------------------------------------------------------------------------
-- Player stats

function changePlayerStat(stat)
  return
    function(game,player,diff)
      local pstate      = getPlayerState(game,player)
      local a           = pstate[stat] + diff
      pstate[stat]      = a

      editBox(GUI.players[player][stat], a)
    end
end


function changeFactionStat(stat)
  return
    function(game,player,faction,diff)
      local pstate      = getPlayerState(game,player)
      local f           = pstate.factions[faction]
      local a           = f[stat] + diff
      f[stat]           = a
      editBox(GUI.players[player].factions[faction][stat],
                                                factionValueLabel(stat,f))
      if factionArmySize(f) <= 0 then
        doRemoveArmy(game,player,faction)
      end
    end
end

changeAvailableWorkers = changePlayerStat("available")
changeCasualties       = changePlayerStat("casualty")
changeFortifications   = changePlayerStat("fortifications")

changeEliteArmy        = changeFactionStat("eliteArmy")
changeMainArmy         = changeFactionStat("mainArmy")
changeLevy             = changeFactionStat("levy")
changeMovement         = changeFactionStat("movement")
changeTreasury         = changeFactionStat("treasury")
changeVP               = changeFactionStat("vp")

function changeRoyalty(game,player,faction,val)
  getPlayerState(game,player).factions[faction].royalty = val
  changeEliteArmy(game,player,faction,0)
  -- change by 0 to force redraw and check for army destroyed
end


--------------------------------------------------------------------------------
-- Bulgars

function changeBulgars(game,diff)
  local x = game.bulgarArmy + diff
  if     x < 0  then x = 0
  elseif x > 11 then x = 11
  end
  game.bulgarArmy = x
  editBox(GUI.bulgarArmy,x.."")
end


--------------------------------------------------------------------------------
-- Armies

function doPlaceArmy(game,player,city,k)
  local faction = getCity(game,city).faction
  getPlayerState(game,player).factions[faction].fieldArmy = city
  spawnArmy(game,player,city,function(o)
    GUI.players[player].factions[faction].fieldArmy = o
    local lab = string.format("  * %s army is now in %s"
                             , faction_poss[faction]
                             , city)
    say(lab)
    k(o)
  end)
end

function doMoveArmy(g,player,faction,city)
  local fstate = getPlayerState(g,player).factions[faction]
  local curCity = fstate.fieldArmy
  fstate.fieldArmy = city

  local newLoc = armyPos(g,player,city)
  local o = GUI.players[player].factions[faction].fieldArmy
  o.setPositionSmooth(newLoc,false,false)
end

function doRemoveArmy(g,player,faction)
  local fstate = getPlayerState(g,player).factions[faction]
  if fstate.fieldArmy ~= nil then
    getPlayerState(g,player).factions[faction].fieldArmy = nil
    local f = GUI.players[player].factions[faction]
    f.fieldArmy.destroy()
    f.fieldArmy = nil
    say(string.format( "%s's %s army was destroyed."
                     , playerColorBB(player), faction_poss[faction]
                     ))
  end
end

function doDestroyArmy(game,player,faction)
  local fstate = getPlayerState(game,player).factions[faction]
  changeEliteArmy(game,player,faction,-fstate.eliteArmy)
  changeMainArmy(game,player,faction,-fstate.mainArmy)
  changeMovement(game,player,faction,-fstate.movement)
  changeRoyalty(game,player,faction,false)
  doRemoveArmy(game,player,faction)
end



--------------------------------------------------------------------------------
-- Cities

function doGainControl(game,player,city,k)
  say(string.format("  * to claim %s", city))

  local cstate = game.map.cities[city]

  -- VP equal to the city's strength
  changeVP(game,player,cstate.faction,cstate.strength)
  say(string.format("  * +%d %s VP"
                   , cstate.strength
                   , faction_poss[cstate.faction]))

  -- mark as controlled
  cstate.controlledBy = player
  redrawCity(game,city,function()

    -- check to see if we should place byzantium's army
    if cstate.faction == byzantium then
      local pstate = getPlayerState(game,player)
      local fstate = pstate.factions[byzantium]
      if fstate.fieldArmy == nil and
         factionArmySize(fstate) > 0 and
         fstate.firstArmyPlacement then
         doPlaceArmy(game,player,city,function()
           fstate.firstArmyPlacement = false
           k()
         end)
      else
        k()
      end
    else
      k()
    end
  end)
end

