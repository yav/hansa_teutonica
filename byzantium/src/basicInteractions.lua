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
--[[
  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]
  local done = 1

  local function doOne()
    if done > todo then k(); return end

    local opts = {}
    for _,stat in ipairs({"movement","mainArmy","eliteArmy"}) do
      if fstate[stat] > 0 then
        opts[stat] =
          function()
            changeFactionStat(game,player,faction,stat,-1)
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
      local lab = string.format("Choose casualty %d/%d",done,todo)
      local cubeOpts = {}
      cubeOpts[faction] = opts
      askCube(game,player,lab,cubeOpts,|f|f())
    end
--]]
end


function chooseRetreat(game,player,faction,city,k)
  local pstate = getPlayerState(game,player)
  local limit  = factionArmySize(pstate.factions[faction])
  local banned = {}
  local alwaysNo = false
  local interact

  local function askPermission(city,cost)
    local byzFleet = game.actionSpaces[byz_fleet]
    askText(game,byzFleet,"Allow sea retreat to " .. city .. "?"
               , { { text = "Yes", val = ||k(city,cost) }
                 , { text = "No"
                   ,  val = function() banned[city] = true; interact() end
                   }
                 , { text = "Disallow any sea retreat"
                   , val = function() alwaysNo = true; interact() end
                   }
                 }
               , |f|f()
               )
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
      askCity(game,player,"Choose city to retreat to:",qopts,{},|f|f())
    else
      k(nil,nil)
    end
  end

  interact()
end

