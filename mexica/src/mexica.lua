
function newGame(ps)
  shuffle(ps)
  local playerState = {}
  for i,p in ipairs(ps) do
    playerState[p] = newPlayer(i,p)
  end

  local map = newMap()
  local startLocs = { location(8,9)
                    , location(9,8)
                    , location(9,10)
                    , location(10,9)
                    }
  locMapLookup(map,location(9,9)).entity = entPalace()
  for _,l in ipairs(startLocs) do
    locMapLookup(map,l).entity = entPalace()
  end

  local allDistricts = { 3,3,4,4,5,5,6,6,7,8,9,10,11,12,13}
  shuffle(allDistricts)

  local districts = {}
  for i = 1,8 do
    districts[i] = allDistricts[i]
  end

  local districtsNext = {}
  for i = 9,15 do
    districtsNext[i-8] = allDistricts[i]
  end
  table.sort(districts)
  table.sort(districtsNext)

  return
    { players       = ps      -- in turn order
    , playerState   = playerState
    , currentPlayer = 1

    , phase         = setup

    , bridges       = 11
    , saveAction    = 12
    , canal1        = 6
    , canal2        = 35
    , districts     = districts
    , districtsNext = districtsNext
    , established   = {}

    , map           = map
    , mapEdges      = findRegion(map,location(1,1))
    , mapStartLocs  = startLocs
    }
end



--------------------------------------------------------------------------------
function terLand()
  return { terrain = land
         , leader = nil
         , entity = nil
         , bridgeFoundation = 0 -- how many bridges are we a foundation for
         , inDistrict = false
         }
end

function terCanal()
  return { terrain = canal, leader = nil, entity = nil }
end
--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Entities

function entTemple(owner,level)
  return { entity = temple, owner = owner, level = level }
end

function entDistrict(size)
  return { entity = district, size = size }
end

function entPalace()
  return { entity = palace }
end

function entBridge(dir)
  return { entity = bridge, direction = dir }
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
    , savedAP = 0
    , leader = nil
    , temples = { 3, 3, 2, 1 }
    , turnAP = 0
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



