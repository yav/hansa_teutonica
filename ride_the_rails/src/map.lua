function newMap()
  return
    { locations = locMapEmpty()

    , routes = {} -- maps companies to the set of locations with their trains

    , disconnected = locMapEmpty()
      -- maps a location to the neighbours that are disconnected from it. 
      -- since walls are generally two way, each wall should appear twice
      -- in the map once for each direction.


      -- this is to support multi-hex regions, such as Berlin
      -- the key of the map is the 
    , largeRegion = locMapEmpty()   -- maps cities to all its hexes
    , suburbRep   = locMapEmpty()   -- maps a city hex to its represnetative lco
    }
end


-- Get the region containing the given location.
function getRegion(map,loc)
  local r = locMapLookup(loc,map.largeRegion)
  if r ~= nil then return r end

  local spot = locMapLookup(map.locations,loc)
  if spot.terrain == terrainSuburb then
    return getRegion(map, locMapLookup(map.suburbRep,loc))
  end

  r = locMapEmpty()
  locMapInsert(r,loc,true)
  return r
end


function addWall(map,loc,dir)
  local walls = map.disconnected
  local other = neighbourLoc(loc,dir)
  local xs = locMapLookup(walls,loc)
  if xs == nil then xs = locMapEmpty(); locMapInsert(walls,loc,xs) end
  locMapInsert(xs,other,true)

  local xs = locMapLookup(walls,other)
  if xs == nil then xs = locMapEmpty(); locMapInsert(walls,other,xs) end
  locMapInsert(xs,loc,true)
end


-- Turn the given location into a mult-hex location, containing the locations
-- described by `others`
function makeLarge(map,loc,others)
  local r = locMapEmpty()
  locMapInsert(r,loc,true)

  local locs = map.locations
  local subs = map.suburbRep

  for _,l in ipairs(others) do
    locMapInsert(r,l,true)
    locMapInsert(subs,l,loc)
    locMapInsert(locs,l,newSpot(terrainSuburb))
  end
  locMapInsert(map.largeRegion,loc,r)
end





function newSpot(terrain)
  return
    { terrain       = terrain
    , trains        = {}      -- who has a train here
    , trainLimit    = 0       -- how many trains can fit here
    , railroadLimit = nil     -- type of city, used to restrict 

    , mayStart      = {}      -- who may start here
    , passenger     = false   -- is there a passenger ehre
    , autoSapwn     = false   -- should we add a passenger if empty

    , bonus         = 0
    , bonusType     = nil
    }
end

function trainNum(spot)
  local count = 0
  for _ in pairs(spot.trains) do
    count = count + 1
  end
  return count
end

function hasSpace(spot)
  return trainNum(spot) < spot.trainLimit
end


