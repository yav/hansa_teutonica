function nextTurn(g)
  turnSave = JSON.encode(g)

  g.turn = g.turn + 1

  local pn = g.curPlayer + 1
  if pn > #g.players then pn = pn - #g.players end
  g.curPlayer = pn
  local p = g.players[pn]

  say("\n Turn #" .. g.turn)
  startTurn(g,p)
  takeActions(g)
end

function endAction(g,s)
  s.turnUsedActions = s.turnUsedActions + 1
  if g.endGame then finalScoring(g) else takeActions(g) end
end

--------------------------------------------------------------------------------

function checkCanPlace(g,p,opts)
  local s = g.playerState[p]
  if s.active[trader] + s.active[merchant] == 0 then return end
  local shape = (s.active[merchant] > 0) and merchant or trader
  if #freeSpots(g,shape,accessibleRegions(g,p)) > 0 then
    push(opts, { text = "Place worker"
               , val  = || actPlaceActiveWorker(g,p)
               })
  else
    log("NO FREE SPOTS?")
  end
end


function checkCanReplace(g,p,opts)
  local s = g.playerState[p]
  local totalActive = s.active[trader] + s.active[merchant]
  if totalActive < 2 then return end

  local onlyTrader  = totalActive == 2
  local onlyRoad    = s.active[merchant] == 0
  local regions     = accessibleRegions(g,p)
  local opSpots     = opponentSpots(g,p,onlyTrader,onlyRoad,regions)
  if #opSpots > 0 then
    push(opts, { text = "Replace opponent"
               , val  = || actReplaceOpponent(g,p,opSpots)
               })
  end
end


function checkCanMove(g,p,opts)
  local anyRegion = nil
  local ourSpots = occupiedSpots(g,p,anyRegion)
  if #ourSpots > 0 then
    push(opts, { text = "Move workers"
               , val  = || actMoveWorkers(g,p,ourSpots)
               })
  end
end


function checkCanComplete(g,p,opts)
  local completed = completedEdges(g,p)
  if #completed > 0 then
    push(opts, { text = "Complete route"
               , val = || actCompleteRoute(g,p,completed)
               })
  end
end


function checkCanHire(g,p,opts)
  local s = g.playerState[p]
  if s.passive[trader] + s.passive[merchant] > 0 then
    push(opts, { text = "Hire workers"
               , val  = || actHireWorkers(g,p)
               })
  end
end

function eotPlaceBonus(g,p)
  local s    = g.playerState[p]
  local todo = #s.turnReplaceBonus

  if s.turnDoneReplaceBonus == todo then
    if todo == 0 then nextTurn(g); return
    else
      local opts = { { text = "End turn", val = nil } }
      askText(g,p,nil,opts,||nextTurn(g))
      return
    end
  end

  local nextTok = s.turnDoneReplaceBonus + 1
  local b       = s.turnReplaceBonus[nextTok]
  local msg     = string.format("Place token %d/%d",nextTok,todo)

  local opts = {}
  for _,edge in ipairs(g.map.edges) do
    if edgeAcceptsBonus(g,edge) then push(opts,edge.id) end
  end

  local menu = menuLoc[g.map.orientation]
  local here = { menu[1], boardPieceZ, menu[2] - 3 }
  local img
  local q = actQ()
  q.enQ(function () img = spawnBonus(g.map,b,here,180,q.next) end)
  q.enQ(||askEdge(g, p, msg, opts, q.next))
  q.enQ(function() checkPoint(g); doPlaceBonus(g,p,b,q.ans().id,q.next) end)
  q.enQ(function()
          img.destroy()
          s.turnDoneReplaceBonus = nextTok
          takeActions(g)
        end)
end



function takeActions(g)
  maybeHideUndo(g)

  local p      = g.players[g.curPlayer]
  local s      = g.playerState[p]

  if s.turnEnded then eotPlaceBonus(g,p); return end

  local remain = s.turnActions - s.turnUsedActions

  local menus = {}

  local opts = {}
  if remain > 0 and not s.turnEnded then
    checkCanPlace(g,p,opts)
    checkCanReplace(g,p,opts)
    checkCanMove(g,p,opts)
    checkCanComplete(g,p,opts)
    checkCanHire(g,p,opts)
  end

  if remain == 0 or #opts == 0 then
    local msg = (#s.turnReplaceBonus > 0) and "End turn (place bonus)"
            or "End turn"
    push(opts, { text = msg
               , val  = function()
                          s.turnEnded = true
                          eotPlaceBonus(g,p)
                        end
               })
  end

  local msg = string.format("%s has %d actions.", playerColorBB(p), remain)
  push(menus, { question = msg, choices = opts })

  local opts1 = {}
  checkBonusUpgradeSkill(g,p,opts1)
  checkBonusSwap(g,p,opts1)
  checkBonusMove(g,p,opts1)
  checkBonusAct(g,p,bonusAct3,opts1)
  checkBonusAct(g,p,bonusAct4,opts1)
  push(menus, { question = "Use bonus", choices = opts1 })

  say("---")
  askTextMany(g,p,menus,function(f) checkPoint(g); f() end)
end





--------------------------------------------------------------------------------
-- Placing workers

-- Assumes at least one active worker and one action
function actPlaceActiveWorker(g,p)
  say(playerColorBB(p) .. " chose to place a worker.")

  local s = g.playerState[p]
  local traderNum = s.active[trader]
  local merchantNum = s.active[merchant]

  local workerType = nil
  local reg = accessibleRegions(g,p)

  local q = actQ()
  q.enQ(|| askWorkerType(g,p,"Choose worker type",s.active, q.next))
  q.enQ(function()
    workerType = q.ans()
    askFreeSpot(g,p, "Place a " .. workerName(workerType)
                         , { owner = p, shape = workerType }, reg, q.next)
  end)

  local buildOn
  q.enQ(function ()
          buildOn = q.ans()
          local e = getEdge(g.map,buildOn.edge)
          doPlaceActive(g, buildOn, {owner=p,shape=workerType},q.next)
        end)
  q.enQ(function() noteBuiltOn(g,p,buildOn.edge); endAction(g,s) end)
end



--------------------------------------------------------------------------------
-- Replacing Workers


-- Put additional workers out when we were displaced
function placeCompWorkers(g,p,e,todo,k)
  if todo == 0 then k(); return; end

  local s = g.playerState[p]

  local function placing(doP)
    return function(t)
      if not t then k(); return; end    -- pased
      local w = {owner=p,shape=t}
      askFreeAdjacent(g,p,"Choose location",e,w,function(loc)
        doP(g,loc,w,||placeCompWorkers(g,p,e,todo-1,k))
      end)
    end
  end

  if s.passive[trader] + s.passive[merchant] > 0 then
    askWorkerTypeOrPass(g,p,"Place passive?",s.passive,placing(doPlacePassive))

  elseif s.active[trader] + s.active[merchant] > 0 then
    askWorkerTypeOrPass(g,p,"Place active?",s.active,placing(doPlaceActive))

  else
    moveCompWorkers(g,p,e,todo,k)
  end
end


-- This happens in the rare case that all your workers are on the board
function moveCompWorkers(g,p,e,todo,k)
  local edge = getEdge(g.map,e)
  local r = nil
  if edge.region ~= g.map.defaultRegion then
    r = {}
    r[edge.region] = true
  end
  local spots = occupiedSpots(g,p,r)
  if #spots == 0 then k(); return; end
  askOccupiedSpotOrPass(g,p,"Move worker?",spots,function(from)
    if not from then k(); return; end    -- pased
    local w = from.worker
    askFreeAdjacent(g,p,"Choose location",e,w,function(to)
      doMoveWorker(g,from,to,||placeCompWorkers(g,p,e,todo-1,k))
    end)
  end)
end

function actReplaceOpponent(g,p,spots)
  say(playerColorBB(p) .. " chose to replace an opponent's worker.")

  local s = g.playerState[p]
  local loc
  local cost
  local oppS

  -- Choose who to bump
  local q = actQ()
  q.enQ(||askOccupiedSpot(g,p,"Replace",spots,q.next))
  q.enQ(function()
    loc = q.ans()
    w = loc.worker
    cost = 1
    if w.shape == merchant then cost = 2 end
    oppS = g.playerState[w.owner]
    q.next()
  end)

  -- They choose new location
  q.enQ(||askFreeAdjacent(g,w.owner,"New location?",loc.edge,w,q.next))
  q.enQ(function()
    local spot = q.ans()
    doMoveWorker(g,loc,spot,q.next)
  end)

  q.enQ(||placeCompWorkers(g,w.owner,loc.edge,cost,q.next))

  -- what are we placing
  local newWT
  q.enQ(function()
    local ty = g.map.edges[loc.edge].stops[loc.stop].type
    if ty == stopShip then
      q.next(merchant)
    else
      askWorkerType(g,p,"Place worker",s.active,q.next)
    end
  end)

  -- place worker
  q.enQ(|| doPlaceActive(g,loc,{owner=p,shape=q.ans()},q.next))
  q.enQ(function() noteBuiltOn(g,p,loc.edge);q.next() end)

  -- pay extra cost
  q.enQ(function()
    -- Pay active workers
    local q1 = actQ()
    for i = 1,cost do
      q1.enQ(|| askWorkerType(g,p,"Make passive",s.active,q1.next))
      q1.enQ(function()
        local t = q1.ans()
        doChangeActive(g,p,t,-1)
        doChangePassive(g,p,t,1)
        q1.next()
      end)
    end
    q1.enQ(q.next)
  end)

  q.enQ(||endAction(g,s))
end





--------------------------------------------------------------------------------
-- Move workers

function moveWorkers(g,p,n,defaultOk,spots,k)
  local passed = false


  -- choices for pickup
  local curSpots = spots
  local function rmSpot(sp)
    local new = {}
    for _,x in ipairs(curSpots) do
      if x.edge ~= sp.edge or x.stop ~= sp.stop then
        push(new,x)
      end
    end
    curSpots = new
  end

  -- these we've picked up
  local pickedUp = {}
  local loc      = menuLoc[g.map.orientation]
  local x,y      = loc[1], loc[2] - 3

  local q = actQ()

  local function pickUp(i)
    if passed then q.next(); return end
    local msg = string.format("Pickup worker %d/%d",i,n)
    askOccupiedSpotOrPass(g,p,msg,curSpots,function(spot)
      if not spot then passed = true; q.next(); return end
      pickedUp[i] = { spot = spot
                    , obj  = doRemoveWorker( g
                                           , spot
                                           , { x + 1.5 * i, boardPieceZ, y })
                    }
      rmSpot(spot)
      q.next()
    end)
  end

  local function putDown(i)
    local info = pickedUp[i]
    if not info then q.next(); return end
    local spot = info.spot
    local obj  = info.obj
    local msg  = string.format("New location for %d/%d",i,#pickedUp)
    local regs = {}
    regs[g.map.edges[spot.edge].region] = true
    if defaultOk then regs[g.map.defaultRegion] = true end
    askFreeSpot (g, p, msg, spot.worker, regs, function(newLoc)
      doPlaceExistingWorker(g,newLoc,spot.worker,obj)
      local eFrom = getEdge(g.map,spot.edge)
      local eTo   = getEdge(g.map,newLoc.edge)
      local note = string.format("%s moved a %s from %s-%s to %s-%s."
                                , playerColorBB(p)
                                , workerName(spot.worker.shape)
                                , eFrom.from, eFrom.to
                                , eTo.from,   eTo.to
                                )
      say(note)
      q.next()
    end)
  end


  for i = 1,n do
    q.enQ(||pickUp(i))
  end

  for i = 1,n do
    q.enQ(||putDown(i))
  end

  q.enQ(k)

end

function actMoveWorkers(g,p,ourSpots)
  say(playerColorBB(p) .. " chose to move workers.")
  local s = g.playerState[p]
  local maxMove = bookLevelMap[s.bookLevel]
  moveWorkers(g,p,bookLevelMap[s.bookLevel], true, ourSpots, ||endAction(g,s))
end



--------------------------------------------------------------------------------
-- Complete a route

function actCompleteRoute(g,p,edges)
  local s = g.playerState[p]
  local q = actQ()
  local edge
  q.enQ(||askEdge(g, p, "Choose route", edges, q.next))

  -- Controlling players of neighbouring cities score a point.
  q.enQ(function()
    edge = q.ans()
    say(string.format("%s completed %s-%s."
                     , playerColorBB(p), edge.from, edge.to
                     ))
    local c = getController(g,edge.from)
    if c then doScorePoints(g,c,1) end
    local c = getController(g,edge.to)
    if c then doScorePoints(g,c,1) end
    q.next()
  end)

  -- Build office or do city action
  q.enQ(||performCompleteRouteAction(g,p,edge,q.next))

  q.enQ(||usePrintedBonusEarly(g,p,edge,q.next))

  -- Return workers from the route back to the supply
  q.enQ(function()
    for _,stop in ipairs(edge.stops) do
      if stop.worker then
        local w = stop.worker
        doRemoveWorker(g,{edge=edge.id, stop = stop.id})
        doChangePassive(g,p,w.shape,1)
      end
    end
    q.next()
  end)

  -- Pick up bonus token. This is done last as the bonus tokens cannot be
  -- used to help with office construction.
  q.enQ(function()
    if     not edge.bonus            then q.next()
    elseif edge.bonus < printedBonus then doTakeBonus(g,p,edge.id,q.next)
    else                                  usePrintedBonus(g,p,edge.bonus,q.next)
    end
  end)

  q.enQ(||endAction(g,s))
end

function checkBuildOffice(g,p,edge,n,opts,k)
  local s = g.playerState[p]

  local off = nextFreeOffice(g,n)
  if not off or off.level > s.buildingLevel then return end

  -- do we have the right shape worker on the rout?
  local have = false
  for _,stop in ipairs(edge.stops) do
    if stop.worker.shape == off.shape then have = true; break; end
  end
  if not have then return end

  push(opts, { text = "Office in " .. n
             , val  = ||doBuildOffice(g,n,edge.id,k)
             })
end

function checkBuildBonusOffice(g,p,edge,n,opts,k)
  local ix = haveBounusToken(g,p,bonusExtra)
  if not ix then return end

  local lab = bonusName(g.map,bonusExtra)

  local node = g.map.nodes[n]
  if #node.offices ~= 0 and node.offices[1].worker == nil then return end

  local function useToken()
    local ws = {}
    local spots = {}
    for i,stop in ipairs(edge.stops) do
      local w = stop.worker
      if w and not ws[w.shape] then
        push(spots, {worker=w, edge=edge.id, stop=stop.id})
        ws[w.shape] = true
      end
    end

    local function doBuilid(spot)
      doUseUpBonus(g,p,ix)
      say(playerColorBB(p) .. " used " .. lab)
      doRemoveWorker(g,spot)
      doAddExtra(g,n,spot.worker,k)
    end

    if #spots > 1 then
      askOccupiedSpotL(g,p,"?","Use worker",spots,doBuilid)
    else doBuilid(spots[1]) end
  end

  push(opts, { text = "Annex in " .. n, val = useToken })
end

function checkCityAction(g,p,edge,n,opts,k)
  local node = g.map.nodes[n]
  local acts = node.action
  if #acts == 0 then return end

  local s = g.playerState[p]

  for _,act in ipairs(acts) do
    local lab = cityActionName[act]
    local function addOpt(f)
      push(opts, { text = lab
                 , val = function() f(); k() end
                 })
    end

    if     act == upgradeAction then
      if s.actionLevel == #actionLevelMap then return end
      addOpt(||doUpgradeAction(g,p))

    elseif act == upgradeBook then
      if s.bookLevel == #bookLevelMap then return end
      addOpt(||doUpgradeBook(g,p))

    elseif act == upgradeKey then
      if s.keyLevel == #keyLevelMap then return end
      addOpt(||doUpgradeKey(g,p))

    elseif act == upgradeBag then
      if s.bagLevel == #bagLevelMap then return end
      addOpt(||doUpgradeBag(g,p))

    elseif act == upgradeBuilding then
      if s.buildingLevel == #buildingLevelMap then return end
      addOpt(||doUpgradeBuilding(g,p))

    elseif act == invest then
      local yes = checkCanInvest(g,p,edge)
      if not yes then return end
      push(opts, { text = lab
                 , val  = ||makeInvestment(g,p,edge,yes,k)
                 })
    else log("UNKNOWN ACTION: " .. n); return end
  end
end

function makeInvestment(g,p,edge,yes,k)
  askInvestmentSpot(g,p,yes.opts,function(i)
    doRemoveWorker(g, {edge=edge.id,stop=yes.stop})
    doAddInvest(g,p,i,k)
  end)
end


function checkCanInvest(g,p,edge)
  local loc   = nil
  local opts  = {}

  -- Do we have a merchant on the route?
  for i,stop in ipairs(edge.stops) do
    if stop.worker and stop.worker.shape == merchant then loc = i; break end
  end
  if not loc then return nil end

  -- Spots that we qualify for
  local s = g.playerState[p]
  for i,_ in ipairs(buildingLevelMap) do
    if i > s.buildingLevel then break end
    local w = g.map.endGameInvest[i]
    if not w then
      push(opts,i)
    end
  end
  if #opts == 0 then return nil end

  return { opts = opts, stop = loc }
end


function performCompleteRouteAction(g,p,edge,k)
  local menus = {}

  local opts = {}
  checkBuildOffice(g,p,edge,edge.from,opts,k)
  checkBuildOffice(g,p,edge,edge.to,opts,k)
  checkCityAction(g,p,edge,edge.from,opts,k)
  checkCityAction(g,p,edge,edge.to,opts,k)
  push(opts, { text = "Do nothing", val = k })
  push(menus, { question = "Choose action", choices = opts })

  local opts1 = {}
  checkBuildBonusOffice(g,p,edge,edge.from,opts1,k)
  checkBuildBonusOffice(g,p,edge,edge.to,opts1,k)
  push(menus, { question = "Use bonus", choices = opts1 })

  askTextMany(g,p,menus,|f|f())
end


--------------------------------------------------------------------------------
-- Hire workers

function actHireWorkers(g,p)
  say(playerColorBB(p) .. " chose to hire workers.")
  local s     = g.playerState[p]
  local limit = bagLevelMap[s.bagLevel]
  local ts    = s.passive[trader]
  local ms    = s.passive[merchant]
  if ts + ms <= limit then
    doChangePassive(g,p,trader,-ts)
    doChangeActive(g,p,trader,ts)
    doChangePassive(g,p,merchant,-ms)
    doChangeActive(g,p,merchant,ms)
    for i = 1,ts do
      say(string.format("%s hired a traders.", playerColorBB(p)))
    end
    for i = 1,ms do
      say(string.format("%s hired a merchant.", playerColorBB(p)))
    end
    endAction(g,s)
    return
  end

  local q = actQ()

  local function hireOne()
    askWorkerType(g,p,"Hire worker",s.passive,function(t)
      doChangePassive(g,p,t,-1)
      doChangeActive(g,p,t,1)
      say(string.format("%s hired a %s.", playerColorBB(p), workerName(t)))
      q.next()
    end)
  end

  for i = 1,limit do
    q.enQ(hireOne)
  end
  q.enQ(||endAction(g,s))
end


--------------------------------------------------------------------------------
-- Using Bonus Tokens

function usePrintedBonusEarly(g,p,e,k)
  local b = e.bonus
  if b == bonusPrintedReuse2 then doBonusPrintedReuse2(g,p,e,k)
  else k()
  end
end

function usePrintedBonus(g,p,b,k)
  if     b == bonusPrintedMove2         then doBonusPrintedMove2(g,p,k)
  elseif b == bonusPrintedPlace2        then doBonusPrintedPlace2(g,p,k)
  elseif b == bonusPrintedGainPrivilege then doBonusPrintedGainPrivilege(g,p,k)
  elseif b == bonusPrintedBuildInGreen  then doBonusPrintedBuildInGreen(g,p,k)
  else k()
  end
end

function checkBonusAct(g,p,b,opts)
  local ix = haveBounusToken(g,p,b)
  if not ix then return end
  local lab = bonusName(g.map,b)

  local function useBonus()
    local s = g.playerState[p]
    local n = (b == bonusAct3) and 3 or 4
    s.turnActions = s.turnActions + n
    say(string.format("Bonus: %s gained %d actions.", playerColorBB(p), n))
    doUseUpBonus(g,p,ix)
    takeActions(g)
  end

  push(opts, { text = lab, val = useBonus })
end


function checkBonusMove(g,p,opts)
  local ix = haveBounusToken(g,p,bonusMove)
  if not ix then return end

  if g.map.modifiedRemove
    then doBonusMove(g,p,ix,opts)
    else doBonusRemove(g,p,ix,opts)
  end
end

function doBonusRemove(g,p,ix,opts)
  local cs = occupiedSpots(g,nil,nil)
  if #cs == 0 then return end
  local lab = bonusName(g.map,bonusMove)

  local n = 3

  local function useBonus()
    say(playerColorBB(p) .. " used " .. lab)
    local q = actQ()
    local passed = false

    local function pickUp(i)
      if passed then q.next(); return end
      local cs = occupiedSpots(g,nil,nil)
      if #cs == 0 then passed = true; q.next(); return end
      local msg = string.format("Remove worker %d/%d",i,n)
      askOccupiedSpotOrPass(g,p,msg,cs,function(spot)
        if not spot
          then passed = true
          else doRemoveWorker(g,spot)
               local e = getEdge(g.map,spot.edge)
               local w = spot.worker
               say(string.format("%s removed a %s %s from %s-%s."
                                        , playerColorBB(p)
                                        , playerColorBB(w.owner)
                                        , workerName(w.shape)
                                        , e.from, e.to))
               doChangeActive(g,w.owner,w.shape,1)
        end
        q.next()
      end)
    end

    for i = 1,n do
      q.enQ(||pickUp(i))
    end
    q.enQ(function()
      doUseUpBonus(g,p,ix)
      takeActions(g)
    end)
  end

  push(opts, { text = lab, val = useBonus })
end

function doBonusMove(g,p,ix,opts)
  local cs = opponentSpots(g,p,false,false,nil)
  if #cs == 0 then return end


  local lab = bonusName(g.map,bonusMove)

  local function useBonus()
    say(playerColorBB(p) .. " used " .. lab)

    local q = actQ()
    local passed = false

    local function putDown(from)
      local edge = g.map.edges[from.edge]
      local w    = from.worker

      local regs = {}
      regs[edge.region] = true

      askFreeSpot(g,p,"New location",w,regs,function(to)
        doPlaceWorker(g,to,w,q.next)
        local toe = g.map.edges[to.edge]
        say(string.format("%s moved a %s %s from %s-%s to %s-%s."
                         , playerColorBB(p), w.owner, workerName(w.shape)
                         , edge.from, edge.to, toe.from, toe.to))
      end)
    end

    local function pickUp(i)
      if passed then q.next(); return end
      local cs = opponentSpots(g,p,false,false,nil)
      if #cs == 0 then q.next(); return end

      local msg = "Move worker " .. i .. "/3"
      askOccupiedSpotOrPass(g,p,msg,cs,function(from)
        if not from then passed = true; q.next() return end
        doRemoveWorker(g,from)
        putDown(from)
      end)
    end

    for i = 1,3 do q.enQ(||pickUp(i)) end
    q.enQ(function()
      doUseUpBonus(g,p,ix)
      takeActions(g)
    end)
  end

  push(opts, { text = lab, val = useBonus })
end



function checkBonusSwap(g,p,opts)
  local ix = haveBounusToken(g,p,bonusSwap)
  if not ix then return end

  local cs = {}
  for n,node in pairs(g.map.nodes) do
    for i,off in ipairs(node.offices) do
      if not off.worker then break end
      if i ~= 1 and off.worker.owner ~= p then
        push(cs, { node = n, office = i, worker = off.worker })
      end
    end
  end

  if #cs == 0 then return end

  local function useBonus()
    askOccupiedSpotL(g,p,"<","Office to move BACK",cs,function(v)
      say(string.format( "Bonus: %s swapped office %d and %d in %s."
                       , playerColorBB(p), v.office - 1, v.office, v.node))

      doSwap(g,v.node,v.office,function()
        doUseUpBonus(g,p,ix)
        takeActions(g)
      end)
    end)
  end

  push(opts, { text = bonusName(g.map,bonusSwap), val = useBonus })
end



function checkBonusUpgradeSkill(g,p,opts)
  local s = g.playerState[p]
  local skills = {}

  local ix = haveBounusToken(g,p,bonusUpgrade)
  if not ix then return end

  local function addOpt(txt,fun)
    push(skills, { text = txt
                , val = function()
                          fun()
                          doUseUpBonus(g,p,ix)
                          takeActions(g)
                        end
                })
  end

  if s.actionLevel < #actionLevelMap then
    addOpt("Actions", ||doUpgradeAction(g,p))
  end

  if s.bookLevel < #bookLevelMap then
    addOpt("Library", ||doUpgradeBook(g,p))
  end

  if s.keyLevel < #keyLevelMap then
    addOpt("Keys", ||doUpgradeKey(g,p))
  end

  if s.bagLevel < #bagLevelMap then
    addOpt("Coffers", ||doUpgradeBag(g,p))
  end

  if s.buildingLevel < #buildingLevelMap then
    addOpt("Privilege", ||doUpgradeBuilding(g,p))
  end

  local useBonus = function()
    say(string.format("Bonus: %s upgrades a skill.", playerColorBB(p)))
    askText(g,p,"Upgrade",skills,|f|f())
  end

  if #skills > 0 then push(opts, { text = bonusName(g.map,bonusUpgrade)
                                 , val = useBonus }) end
end

function doBonusPrintedMove2(g,p,k)
  local n = 2
  say(string.format("Shipping bonus: %s may move %d workers."
                   , playerColorBB(p), n))
  moveWorkers(g,p,n,false,occupiedSpots(g,nil,nil),k)
end

function foreignOptions(m)
  local lab = ""
  for _,r in ipairs(m.regions) do
    if r ~= m.defaultRegion then
      local nm = m.regionNames[r]
      if lab == "" then lab = nm else lab = lab .. "/" .. nm end
    end
  end
  return lab
end

function doBonusPrintedPlace2(g,p,k)
  local n = 2

  local lab = foreignOptions(g.map)

  say(string.format("Shipping bonus: %s may place %d workers in %s."
                   , playerColorBB(p), n, lab))

  local s = g.playerState[p]
  local regs = {}
  for _,r in ipairs(g.map.regions) do
    if r ~= g.map.defaultRegion then
      regs[r] = true
    end
  end

  local q = actQ()
  local passed = false

  local function place(i)
    if passed then q.next(); return end
    local shape = (s.active[merchant] > 0) and merchant or trader
    if #freeSpots(g,shape,regs) == 0 then passed = true; q.next(); return end

    local msg = "Place worker " .. i .. "/" .. n
    askWorkerTypeOrPass(g,p,msg,s.active,function(t)
      if not t then passed = true; q.next(); return end
      local w = { owner = p, shape = t }
      askFreeSpot(g,p,msg,w,regs,|spot| doPlaceActive(g,spot,w,q.next))
    end)
  end

  for i = 1,n do
    q.enQ(||place(i))
  end
  q.enQ(k)
end


function doBonusPrintedGainPrivilege(g,p,k)
  say(string.format("Shipping bonus: %s gain 1 privilage.",p))
  doUpgradeBuilding(g,p)
  k()
end

function doBonusPrintedBuildInGreen(g,p,k)
  say(string.format("Shipping bonus: %s may establish an office in a restricted city.",p))
  local s = g.playerState[p]
  if s.active[trader] + s.active[merchant] == 0 then
    say(string.format("%s has no active workers",p))
    k();
    return
  end

  local function buildIn(n)
    askWorkerType(g,p,"Worker type",s.active,function(t)
      doAddExtraRight(g,n,{owner=p,shape=t},k)
    end)
  end

  local opts = {}
  for n,node in pairs(g.map.nodes) do
    if #node.offices == 0 then
      push(opts, { text = "Office in " .. node.name
                 , val  = ||buildIn(n)
                 })
    end
  end

  push(opts, { text = "Pass", val = k })

  askText(g,p,"Establish office",opts,|f|f())
end



function doBonusPrintedReuse2(g,p,e,k)

  say(string.format( "Shipping bonus: %s may move workers from route."
                   , playerColorBB(p)
                   ))
  local n = 2

  local q = actQ()
  local passed = false

  local function reuse(i)
    if passed then q.next(); return end
    local spots = {}
    occupiedSpotsOn(g,p,e,spots)
    if #spots == 0 then passed = true; q.next() return end
    local msg = string.format("Move worker %d/%d",i,n)
    askOccupiedSpotOrPass(g,p,msg,spots,function(spot)
      if not spot then passed = true; q.next(); return end
      local w = spot.worker
      local regs = nil -- any region (on the East expansion there no regions)
      local tgts = freeSpots(g,w.shape,regs)
      if #tgts == 0 then passed = turn; q.next(); return end
      askFreeSpot(g, p, "New location", w, regs,
                                  |tgt|doMoveWorker(g,spot,tgt,q.next))
    end)
  end

  for i = 1,n do
    q.enQ(||reuse(i))
  end
  q.enQ(k)
end
