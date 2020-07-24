

function endTurn(g,p)

  local s = g.playerState[p]
  local save = 0

  local function doEnd()
    doSaveAP(g,p,save)
    local n = g.currentPlayer
    if n == #g.players then endRound(g); return end
    g.currentPlayer = n + 1
    startTurn(g)
  end

  if s.turnAP > 0 then
    save = s.turnAP
    if save > 2 then save = 2 end
    if save > g.saveAction then save = g.saveAction end
    local opts = { { text = string.format("End turn, saving %d AP",save)
                   , val  = doEnd
                   }
                 , { text = "Take More Actions"
                   , val  = ||takeAction(g,p)
                   }
                 }
    askText(g,p,string.format("%d AP Remaing",s.turnAP),opts,|f|f())
  else
    doEnd()
  end

end


function endAge(g)
  say("End of Age")
  for _,spots in ipairs(g.established) do
    doScoreDistrict(g,spots)
  end
  if g.phase == age2 then
    for _,spots in ipairs(unclaimedDistricts(g.map)) do
      doScoreDistrict(g,spots)
    end
  end
  for p,s in pairs(g.playerState) do
    for _,l in ipairs(g.mapStartLocs) do
      if locationSame(s.leader,l) then doScoreVP(g,p,5) end
    end
  end

  if g.phase == age2 then doEndGame(g); return end
  doEndAge(g,||startRound(g))
end

function endRound(g)
  local endCondition = 0
  if next(g.districts) == nil then endCondition = endCondition + 1 end
  for p,s in pairs(g.playerState) do
    local allTemples = 0
    for _,n in ipairs(s.temples) do allTemples = allTemples + n end
    if allTemples == 0 then endCondition = endCondition + 1; break end
  end
  if endCondition == 2 then endAge(g) else startRound(g) end
end

function startRound(g)
  if g.phase == setup then g.phase = age1; editPhase(g.phase) end
  g.currentPlayer = 1
  startTurn(g)
end

function startTurn(g)
  doSave(g)

  local p       = g.players[g.currentPlayer]
  local s       = g.playerState[p]
  s.turnAP      = g.phase == setup and 0 or 6
  s.turnSavedAP = 0
  takeAction(g,p)
end


function setupPlaceLeader(g,p)
  local spots = locMapEmpty()
  for _,l in ipairs(g.mapStartLocs) do
    if locMapLookup(g.map,l).leader == nil then locMapInsert(spots,l,true) end
  end
  local q = string.format("Place %s leader",playerColorBB(p))
  askMapLoc(g,p,q, spots, nil, function(loc)
    doPlaceLeader(g,loc,p,||endTurn(g,p))
  end)
end


function takeAction(g,p)
  local json = JSON.encode({g,p})

  local s = g.playerState[p]
  if s.leader == nil then setupPlaceLeader(g,p); return end

  local opts = {}

  local apOpts = {}
  checkRestoreAP(g,p,apOpts)
  push(opts, { name = nil, choices = apOpts })

  local buildOpts = {}
  checkCanal(g,p,buildOpts)
  checkBuildBridge(g,p,buildOpts)
  checkMoveBridge(g,p,buildOpts)
  checkDistrict(g,p,buildOpts)
  checkBuildTemple(g,p,buildOpts)
  push(opts, { name = "Build", choices = buildOpts })

  local moveOpts = {}
  checkMove(g,p,moveOpts)
  checkBoat(g,p,moveOpts)
  checkTeleport(g,p,moveOpts)
  push(opts, { name = "Move", choices = moveOpts })

  push(opts, { name = nil
             , choices = { { text = "End Turn", val = ||endTurn(g,p) } }
             })

  local q = string.format("%s has %d AP",playerColorBB(p),s.turnAP)

  askTextMulti(g,p,q,opts,function(f) pushUndo(json); f() end)
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
    q.enQ(||askMapLoc(g,p,"Canal location",canalSpots,nil,function(x)
      spot1 = x
      doPlaceCanal(g,spot1,q.next)
    end))

    if not onlySingle then
      q.enQ(function()
        local spots = freeCanalNeihbours(g.map,spot1)
        if locMapIsEmpty(spots) then q.next(); return end
        local optional = onlyDouble and nil or "Pass"
        askMapLoc(g,p,"Second location?",spots,optional,function(loc)
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
        extra = string.format("and %s",locName(spot2))
      else
        doBuildCanal1x1(g)
      end
      say(string.format("%s built a canal on %s %s"
                       , playerColorBB(p), locName(spot1), extra))
      s.turnAP = s.turnAP - 1
      takeAction(g,p)
    end)
  end


  push(opts, { text = "Canal (1 AP)", val = buildCanal })
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
    askMapLoc(g,p,"New leader location?",spots,nil,function(to)
      doMoveLeader(g,loc,to,function()
        s.turnAP = s.turnAP - locMapLookup(spots,to)
        say(string.format("%s travelled on foot", playerColorBB(p)))
        takeAction(g,p)
      end)
    end)
  end

  push(opts, { text = "On Foot (1 AP/step)", val = move })
end

function checkBoat(g,p,opts)
  local s = g.playerState[p]
  if s.turnAP == 0 then return end

  local loc  = s.leader
  local spot = locMapLookup(g.map,loc)
  if spot.entity == nil or spot.entity.entity ~= bridge then return end

  local limit = s.turnAP
  if limit > 4 then limit = 4 end -- at 5 we can teleport
  local spots = moveByBoat(g.map,loc,limit)
  if locMapIsEmpty(spots) then return end

  local function move()
    askMapLoc(g,p,"New leader location?",spots,nil,function(to)
      doMoveLeader(g,loc,to,function()
        s.turnAP = s.turnAP - locMapLookup(spots,to)
        say(string.format("%s travelled by boat", playerColorBB(p)))
        takeAction(g,p)
      end)
    end)
  end

  push(opts, { text = "By Boat (1 AP/bridge)", val = move })

end

function checkTeleport(g,p,opts)
  local s = g.playerState[p]
  if s.turnAP < 5 then return end
  local spots = teleportSpots(g.map)
  if locMapIsEmpty(spots) then return end
  local function move()
    askMapLoc(g,p,"New leader location?",spots,nil,function(to)
      doMoveLeader(g,s.leader,to,function()
        s.turnAP = s.turnAP - 5
        say(string.format("%s travelled by air", playerColorBB(p)))
        takeAction(g,p)
      end)
    end)
  end

  push(opts, { text = "By Air (5 AP)", val = move })
end





function checkBuildBridge(g,p,opts)
  if g.bridges <= 0 then return end

  local s = g.playerState[p]
  if s.turnAP == 0 then return end

  local spots = bridgeSpots(g.map)
  if locMapIsEmpty(spots) then return end

  local function build()
    local qs = locMapEmpty()
    for l,_ in locsIn(spots) do
      locMapInsert(qs,l,true)
    end
    askMapLoc(g,p,"Bridge location?",qs,nil,function(loc)
      local dirs = locMapLookup(spots,loc)
      local function doIt(dir)
        doBuildBridge(g,loc,dir,function()
          s.turnAP = s.turnAP - 1
          say(string.format("%s built a bridge on %s",
                          playerColorBB(p), locName(loc)))
          takeAction(g,p)
        end)
      end

      if #dirs == 1 then
        doIt(dirs[1].val)
      else
        askText(g,p,"Bridge direction?",dirs,doIt)
      end
    end)
  end

  push(opts, { text = "Bridge (1 AP)", val = build })
end

function checkBuildTemple(g,p,opts)
  local s = g.playerState[p]

  -- not in a district
  if locMapLookup(g.map, s.leader).terrain == canal then return end
  local spots = templeSpots(g.map, s.leader)
  if locMapIsEmpty(spots) then return end  -- district is full

  local function build(level)
    askMapLoc(g,p,"Temple location?",spots,nil,function(loc)
      doBuildTemple(g,p,loc,level,function()
        s.turnAP = s.turnAP - level
        say(string.format( "%s built a level %d temple on %s"
                         , playerColorBB(p),level,locName(loc)
                         ))
        takeAction(g,p)
      end)
    end)
  end

  if s.turnAP < 1 then return end
  if s.temples[1] > 0 then
    push(opts, { text = "Level 1 Temple (1 AP)", val = ||build(1) })
  end

  if s.turnAP < 2 then return end
  if s.temples[2] > 0 then
    push(opts, { text = "Level 2 Temple (2 AP)", val = ||build(2) })
  end

  if s.turnAP < 3 then return end
  if s.temples[3] > 0 then
    push(opts, { text = "Level 3 Temple (3 AP)", val = ||build(3) })
  end

  if s.turnAP < 4 then return end
  if s.temples[4] > 0 then
    push(opts, { text = "Level 4 Temple (3 AP)", val = ||build(4) })
  end
end

function checkDistrict(g,p,opts)
  local s = g.playerState[p]

  if locMapLookup(g.map, s.leader).terrain == canal then return end
  local spots = findRegion(g.map, s.leader)
  local size = 0
  local tokSpot = locMapEmpty()
  for l,_ in locsIn(spots) do
    size = size + 1
    local spot = locMapLookup(g.map,l)
    if spot.entity == nil and spot.leader == nil then
      locMapInsert(tokSpot,l,true)
    end
  end
  if locMapIsEmpty(tokSpot) then return end -- no space for district token

  local district = 0
  for i,d in pairs(g.districts) do
    if d == size then district = i; break end
  end
  if district == 0 then return end    -- no matching token


  local function establish()
    askMapLoc(g,p,"District Marker?",tokSpot,nil,function(loc)
      doEstablish(g,p,loc,district,spots,function()
        local msg = string.format("%s established a size %d district on %s"
                                 , playerColorBB(p), size, locName(loc))
        say(msg)
        takeAction(g,p)
      end)
    end)
  end

  push(opts, { text = "Distrcit (0 AP)", val = establish })
end

function checkMoveBridge(g,p,opts)
  if g.bridges > 0 then return end
  local s = g.playerState[p]
  if s.turnAP < 1 then return end

  local spots = moveBridgeSpots(g.map)
  if locMapIsEmpty(spots) then return end

  local tgtSpots = bridgeSpots(g.map)
  if locMapIsEmpty(tgtSpots) then return end

  local function move()
    askMapLoc(g,p,"Choose Bridge",spots,nil,function(from)
      local tgts = locMapEmpty()
      for l,_ in locsIn(tgtSpots) do locMapInsert(tgts,l,true) end
      askMapLoc(g,p,"New Bridge Location",tgts,nil,function(to)
        local dirs = locMapLookup(tgtSpots,to)
        local function doIt(dir)
          doMoveBridge(g,from,to,dir,function()
            s.turnAP = s.turnAP - 1
            local msg = string.format( "%s moved a bridge from %s to %s"
                                     , playerColorBB(p)
                                     , locName(from), locName(to))
            say(msg)
            takeAction(g,p)
          end)
        end
        if #dirs == 1 then
          doIt(dirs[1].val)
        else
          askText(g,p,"Bridge direction?",dirs,doIt)
        end

      end)
    end)
  end

  push(opts, { text = "Move Bridge (1 AP)", val = move })
end

function checkRestoreAP(g,p,opts)
  local s = g.playerState[p]
  if s.savedAP <= 0 then return end

  local function restore()
    doRestoreAP(g,p)
    takeAction(g,p)
  end

  push(opts, { text = "Restore 1 AP", val = restore })
end


function locName(loc)
  return string.format("%d,%d",loc.col,loc.row)
end
