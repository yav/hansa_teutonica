
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
end

