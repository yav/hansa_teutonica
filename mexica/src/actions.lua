

function endTurn(g)
  -- XXX: ask question if leftover AP
  local n = g.currentPlayer
  if n == #g.players then endRound(g); return end
  g.currentPlayer = n + 1
  startTurn(g)
end

function endRound(g)
  -- XXX: check for scoring/end game
  g.currentPlayer = 1
  startTurn(g)
end

function startTurn(g)
  local p = g.players[g.currentPlayer]
  local s = g.playerState[p]
  s.turnAP      = 6
  s.turnSavedAP = 0
  takeAction(g,p)
end


function setupPlaceLeader(g,p)
  local spots = locMapEmpty()
  for _,l in ipairs(g.mapStartLocs) do
    if locMapLookup(g.map,l).leader == nil then locMapInsert(spots,l,true) end
  end
  local q = string.format("Place %s leader",playerColorBB(p))
  askMapLoc(p,q, spots, nil, function(loc)
    doPlaceLeader(g,loc,p,||endTurn(g))
  end)
end


function takeAction(g,p)
  local s = g.playerState[p]
  if s.leader == nil then setupPlaceLeader(g,p); return end

  local opts = {}
  checkCanal(g,p,opts)
  checkBuildBridge(g,p,opts)
  -- XXX: move bridge
  checkMove(g,p,opts)
  -- XXX: move by boat
  -- XXX: teleport
  -- XXX: build temple
  -- XXX: establish district
  -- XXX: restore AP
  push(opts, { text = "End turn", val = ||endTurn(g) })

  local q = string.format("%s has %d AP",playerColorBB(p),s.turnAP)
  askText(p,q,opts,|f|f())
end


--------------------------------------------------------------------------------

function checkPlaceLeader(g,p,opts)
  local s = g.playerState[p]
  if s.leader ~= nil then return end    -- only at the start of the game

  push(opts, { text = "Place leader", val = placeLeader })
end


function checkCanal(g,p,opts)
  local onlySingle = g.canal2 == 0
  local onlyDouble = g.canal1 == 0
  if onlyDouble and onlySingle then return end -- no more canals

  local s = g.playerState[p]
  if s.turnAP == 0 then return end    -- we need an action point


  local canalSpots = freeCanalSpots(g.map, onlyDouble)
  if locMapIsEmpty(canalSpots) then return end -- no place on the map

  local function buildCanal()
    local q = actQ()
    local spot1
    local spot2
    q.enQ(||askMapLoc(p,"Canal location",canalSpots,nil,function(x)
      spot1 = x
      doPlaceCanal(g,spot1,q.next)
    end))

    if not onlySingle then
      q.enQ(function()
        local spots = freeLandNeigbours(g.map,spot1,false)
        if locMapIsEmpty(spots) then q.next(); return end
        local optional = onlyDouble and nil or "Pass"
        askMapLoc(p,"Second location?",spots,optional,function(loc)
          if not loc then q.next(); return end
          spot2 = loc
          doPlaceCanal(g,spot2,q.next)
        end)
      end)
    end

    q.enQ(function()
      local extra = ""
      if spot2 then
        doBuildCanal2x1(g)
        extra = string.format("and %d,%d",spot2.col,spot2.row)
      else
        doBuildCanal1x1(g)
      end
      say(string.format("%s built a canal on %d,%d %s"
                       , playerColorBB(p), spot1.col, spot1.row, extra))
      s.turnAP = s.turnAP - 1
      takeAction(g,p)
    end)
  end


  push(opts, { text = "Build canal (1 AP)", val = buildCanal })
end


function checkMove(g,p,opts)
  local s = g.playerState[p]
  if s.turnAP == 0 then return end

  local loc  = s.leader
  local spot = locMapLookup(g.map,loc)


  local limit = s.turnAP
  if limit > 4 then limit = 4 end -- at 5 we can teleport
  local spots = moveOnFoot(g.map,loc,limit)
  if locMapIsEmpty(spots) then return end

  local function move()
    askMapLoc(p,"New leader location?",spots,nil,function(to)
      doMoveLeader(g,loc,to,function()
        s.turnAP = s.turnAP - locMapLookup(spots,to)
        takeAction(g,p)
      end)
    end)
  end

  push(opts, { text = "Move leader (1 AP/step)", val = move })
end


function checkBuildBridge(g,p,opts)
  if g.bridges == 0 then return end

  local s = g.playerState[p]
  if s.turnAP == 0 then return end

  local spots = bridgeSpots(g.map)
  if locMapIsEmpty(spots) then return end

  local function build()
    local qs = locMapEmpty()
    for l,_ in locsIn(spots) do
      locMapInsert(qs,l,true)
    end
    askMapLoc(p,"Bridge location?",qs,nil,function(loc)
      local dirs = locMapLookup(spots,loc)
      local function doIt(dir)
        doBuildBridge(g,loc,dir,function()
          s.turnAP = s.turnAP - 1
          takeAction(g,p)
        end)
      end

      if #dirs == 1 then
        doIt(dirs[1].val)
      else
        askText(p,"Bridge direction?",dirs,doIt)
      end
    end)
  end

  push(opts, { text = "Build bridge (1 AP)", val = build })
end
