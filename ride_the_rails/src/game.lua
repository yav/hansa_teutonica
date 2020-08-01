

function newGame(ctrl,map)
  local players = {}
  for player,ctrlBy in pairs(ctrl) do
    push(players,player)
  end

  shuffle(players)

  local playerState = {}
  for turnOrder,player in ipairs(players) do
    playerState[turnOrder] = newPlayer(turnOrder,player)
  end

  local supply = {}
  for _,name in ipairs(companyName) do
    supply[name] = 27
  end


  return
    { players      = players
    , controlledBy = ctrl
    , playerState  = playerState

    , currentPlayer = 0
    , round         = 0
    , phase         = nil

    , supply        = supply

    , map           = map
    }

end


function newPlayer(turnOrder,color)
  local shares = {}
  for _,name in ipairs(companyName) do
    shares[name] = 0
  end

  return
    { color       = color
    , turnOrder   = turnOrder

    , shares      = shares
    , money       = 0

    , turnBuildAP          = 0      -- remaining locomotives to place in phase 2
    , turnBuiltInMountains = false  -- have we built in mountains yet
    }
end



