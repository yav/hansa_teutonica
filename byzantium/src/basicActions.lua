
--------------------------------------------------------------------------------
-- Actions

function markAction(g,player,action)
  g.actions[action] = player
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
        doRemoveArmy(g,player,army)
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
-- Armies

-- we don't compute the faction from the city, as if the army is
-- attacking then these will no match.
function doPlaceArmy(g,player,faction,city,k)
  getPlayerState(g,player).factions[faction].fieldArmy = city
  spawnArmy(g,player,city,function(o)
    GUI.players[player].factions[faction].fieldArmy = o
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
  getPlayerState(g,player).factions[faction].fieldArmy = nil
  local f = GUI.players[player].factions[faction]
  f.fieldArmy.destroy()
  f.fieldArmy = nil
end



--------------------------------------------------------------------------------
-- Cities

function doGainControl(game,player,city,k)
  say(string.format("%s claimed %s.", playerColorBB(player), city))

  local cstate = game.map.cities[city]

  -- VP equal to the city's strength
  changeVP(game,player,cstate.faction,cstate.strength)
  say(string.format("  * scored %d %s VP."
                   , cstate.strength
                   , faction_name[cstate.faction]))

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
         doPlaceArmy(game,player,byzantium,city,function()
           fstate.firstArmyPlacement = false
           say("  * byzantene army is in the city")
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

