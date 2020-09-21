-- XXX: this is wrong: it is ok to use *any* cube no matter the faction
function workerOptions(game,player,faction)
  local pstate = getPlayerState(game,player)
  local fstate = pstate.factions[faction]

  local opts = {}
  local function label(x,c)
    if c == 0 then return x end
    return string.format("%s ($%s 3)", x, faction == arab and "A" or "B", c)
  end
  local function addP(x,c,f)
    push(opts, { text = label(x,c)
               , val  = function()
                          changeTreasury(game,player,faction,-c)
                          f(game,player,-1)
                        end
               })
  end
  local function addF(x,c,f)
    push(opts, { text = label(x,c)
               , val  = function()
                          changeTreasury(game,player,faction,-c)
                          f(game,player,faction,-1)
                        end
               })
  end



  if pstate.available > 0 then
                        addP("Available worker", 0, changeAvailableWorkers) end
  if fstate.treasury < 3 then return opts end

  if pstate.casualty  > 0 then addP("Hire worker",3,       changeCasualties) end
  if fstate.levy      > 0 then addF("Reduce levy",3,       changeLevy)       end
  if fstate.mainArmy  > 0 then addF("Reduce main army",3,  changeMainArmy)   end
  if fstate.eliteArmy > 0 then addF("Reduce elite army",3, changeEliteArmy)  end
  if fstate.movement  > 0 then addF("Reduce movement",3,   changeMovement)   end

  return opts
end





function checkControlAction(game, opts)
  local player = getCurrentPlayer(game)
  local pstate = getPlayerState(player)

  -- What cities can be controlled by the current player
  local choice = {}
  local payments = {}

  for name,city in pairs(game.map.cities) do
    local faction = mayControlCity(city)
    if faction ~= nil then
      local workerOpts = workerOptions(game,player,faction)
      if #workerOpts > 0 then
        push(choice,name)
        payments[name] = workerOpts
      end
    end
  end
  if #choice == 0 then return end

  push(opts,
    { text = "Take Control of a City"
    , val = function()
              askCity(choice, function(city)
                askTextQuick(payments[city], function(makePayment)
                  makePayment()
                  doGainControl(game,player,city)
                end)
              end)
            end
    })
end
