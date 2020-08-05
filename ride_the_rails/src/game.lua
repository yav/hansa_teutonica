

function newGame(ctrl,map)
  local players   = {}
  local turnOrder = {}
  for player,ctrlBy in pairs(ctrl) do
    push(players,player)
    push(turnOrder,player)
  end
  shuffle(turnOrder)

  local playerState = {}
  for i,player in ipairs(players) do
    playerState[player] = newPlayer(i,player)
  end

  local supply = {}
  for _,name in ipairs(companyName) do
    supply[name] = 27
  end


  return
    { players       = players
    , controlledBy  = ctrl
    , playerState   = playerState

    , turnOrder     = turnOrder
    , currentPlayer = 1                 -- index in turn order
    , round         = 1
    , phase         = phaseTakeShare

    , supply        = supply

    , map           = map
    }

end


function newPlayer(id,color)
  local shares = { }
  for _,name in ipairs(companyName) do
    shares[name] = 0
  end

  return
    { color       = color
    , id          = id

    , shares      = shares
    , money       = 0

    , turnBuildAP          = 0      -- remaining locomotives to place in phase 2
    , turnBuiltInMountains = false  -- have we built in mountains yet
    }
end



