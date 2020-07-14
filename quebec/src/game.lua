-------------------------------------------------------------------------------
-- State

local VERSION = 9

function gsNew()
  local gs = {}

  gs.version = VERSION      -- to avoid save state confusion during development
  gs.turn = 0               -- global turn counter, identifies save states

  gs.pnum = 0               -- how many players we have
  gs.players = {}           -- player colors (serve as ID)
  gs.playerState = {}       -- state for each player, indexed by color

  gs.ageLocs = {}           -- locations part of each age
  gs.buildingFor = {}       -- which building to use when completing loc

  gs.availableSiteNum = 0   -- how many sites are still available to build
  gs.availableSites = {}    -- locations of available sites
  gs.activeSites = {}       -- sites under constructions

  gs.currentAge = 0         -- the current age
  gs.curPlayer = 0      -- the current player

  gs.leaders = {}          -- leaders available for the takings

  gs.mode = 1               -- which flavor of the rules we are using

  gs.baseEvent = nil        -- event from age 1
  gs.baseEvent = nil        -- event from age 1
  gs.wonRace = nil          -- first player to win the baseEvent, if a race
  gs.event = nil            -- event for other ages

  gs.finished = false

  return gs
end


function gsNewPlayer(gs, p)

  local s = {}

  s.owns = {}     -- buildings we completed
  s.score = 0     -- VPS
  s.arch = nil    -- location of architect

  s.active = 3    -- activeA workers

  local pnum = gs.pnum
  s.passive = pnum == 3 and 22 or
              pnum == 4 and 19 or
              pnum == 5 and 17
  -- s.passive = 3 -- XXX

  s.leader = 0
  s.arch2  = nil  -- location of 2nd architect

  -- influence
  local zone = {}
  for i = 1,5 do
    zone[i] = 0
  end
  s.zone = zone

  gs.playerState[p] = s

  return gs
end



function gsSetupPlayers(gs,ps)

  gs.pnum = #ps
  gs.players = ps

  for _,p in ipairs(ps) do
    gsNewPlayer(gs,p)
  end
end



function gsSetupAges(gs)

  -- rotate the entries in ana rray
  local function rotate(inA,i)
    local outA = {}
    local ix = i
    for i,x in ipairs(inA) do
      outA[ix] = x
      ix = ix + 1
      if ix > #inA then ix = 1 end
    end
    return outA
  end

  -- shuffle the locations in a district
  local function shuffleGroup(g)
    local a = {}
    local i = 1
    for _,rs in ipairs(g) do
      for _,c in ipairs(rs.spots) do
        a[i] = c
        i = i + 1
      end
    end
    return shuffle(a)
  end

  local num = {4,2,3,2}

  -- assign buildings to the map
  for i,g in ipairs(spotByColor) do
    local ls = shuffleGroup(g)
    local ix = 1
    for age,n in ipairs(rotate(num,i)) do
      local objs = gs.ageLocs[age]
      if not objs then
        objs = {}
        gs.ageLocs[age] = objs
      end

      for z = 1,n do
        -- local loc = ls[ix]
        gs.buildingFor[ls[ix]] = { type = i, age = age, ix = z }
        push(objs,ls[ix])
        ix = ix + 1
      end
    end
  end

end


function gsCurPlayer(gs)
  return gs.players[gs.curPlayer]
end

function gsNextPlayer(gs)
  local n = gs.curPlayer + 1
  if n > gs.pnum then n = 1 end
  gs.curPlayer = n
end



