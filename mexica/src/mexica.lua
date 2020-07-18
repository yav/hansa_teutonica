
function newGame(ps)
  shuffle(ps)
  local playerState = {}
  for i,p in ipairs(ps) do
    playerState[p] = newPlayer(i,p)
  end

  local map = newMap()

  return
    { players       = ps      -- in turn order
    , playerState   = playerState
    , currentPlayer = 1
    , map           = map
    , mapEdges      = findRegion(map,location(1,1))
    }
end


--------------------------------------------------------------------------------
function terLand()
  return { terrain = land, entity = nil }
end

function terCanal()
  return { terrain = canal, entity = nil, bridge = nil }
end
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Entities

function entLeader(owner)
  return { entity = leader, owner = owner }
end

function entTemple(owner,level)
  return { entity = temple, owner = owner, level = level }
end

function entDistrict(size)
  return { entity = district, size = size }
end

function entPalace()
  return { entity = palace }
end

--------------------------------------------------------------------------------





function emptyMap()
  return {}
end

function addCanal(map,loc)
  locMapInsert(map,loc,terCanal())
end

function addEntity(map,loc,entity)
  local spot  = locMapLookup(map,loc)
  spot.entity = entity
end


--------------------------------------------------------------------------------
-- Players

function newPlayer(turnOrder,color)
  return
    { color = color
    , turnOrder = turnOrder
    , VP = 0
    , AP = 0
    , savedAP = 0
    , leader = nil
    , temples = { 3, 3, 2, 1 }
    }
end


--------------------------------------------------------------------------------
-- Locations

function location(row,col)
  return { row = row, col = col }
end

function locationSame(x,y)
  return x.row == y.row and x.col == y.col
end

function neighbour(loc,dir)
  return location(loc.row + dirDY[dir], loc.col + dirDX[dir])
end



