function nextTurn(g)
  g.turn = g.turn + 1

  local pn = g.curPlayer + 1
  if pn > #g.players then pn = pn - #g.players end
  g.curPlayer = pn
  local p = g.players[pn]

  print("\n Turn #" .. g.turn)
  startTurn(g,p)
  takeActions(g)
end

function endAction(s,k)
  s.turnUsedActions = s.turnUsedActions + 1
  -- XXX: check for end game
  k()
end

function endTurn(g)
  -- XXX: replace bonus marker
  nextTurn(g)
end


function checkCanPlace(g,p,opts)
  local s = g.playerState[p]
  if s.active[trader] + s.active[merchant] == 0 then return end
  local shape = (s.active[merchant] > 0) and merchant or trader
  if #freeSpots(g,shape,accessibleRegions(g,p)) > 0 then
    push(opts, { text = "Place worker"
               , val  = || actPlaceActiveWorker(g,p,||takeActions(g))
               })
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
               , val  = || actReplaceOpponent(g,p,opSpots,||takeActions(g))
               })
  end
end


function checkCanMove(g,p,opts)
  local anyRegion = nil
  local ourSpots = occupiedSpots(g,p,anyRegion)
  if #ourSpots > 0 then
    push(opts, { text = "Move workers"
               , val  = || actMoveWorkers(g,p,ourSpots,||takeActions(g))
               })
  end
end


function checkCanComplete(g,p,opts)
  local completed = completedEdges(g,p)
  if #completed > 0 then
    push(opts, { text = "Complete route"
               , val = || actCompleteRoute(g,p,completed,||takeActions(g))
               })
  end
end


function checkCanHire(g,p,opts)
  local s = g.playerState[p]
  if s.passive[trader] + s.active[trader] > 0 then
    push(opts, { text = "Hire workers"
               , val  = || actHireWorkers(g,p,||takeActions(g))
               })
  end
end



function takeActions(g)
  local p      = g.players[g.curPlayer]
  local s      = g.playerState[p]
  local remain = s.turnActions - s.turnUsedActions


  local opts = {}
  if remain > 0 then
    checkCanPlace(g,p,opts)
    checkCanReplace(g,p,opts)
    checkCanMove(g,p,opts)
    checkCanComplete(g,p,opts)
    checkCanHire(g,p,opts)
  end


  if remain == 0 or #opts == 0 then
    push(opts, { text = "End turn", val = || endTurn(g) })
  end
  -- Use plate
  -- Normal action, if actional point remaining

  local msg = playerColorBB(p) .. " has " .. remain .. " actions"
  print("\n" .. msg)
  askText(p,msg,opts,|f|f())
end








-- Assumes at least one active worker and one action
function actPlaceActiveWorker(g,p,k)
  print(playerColorBB(p) .. " chose to place a worker.")

  local s = g.playerState[p]
  local traderNum = s.active[trader]
  local merchantNum = s.active[merchant]

  local workerType = nil
  local reg = accessibleRegions(g,p)

  local q = actQ()
  q.enQ(|| askWorkerType(p,"Choose worker type",s.active, q.next))
  q.enQ(function()
    workerType = q.ans()
    askFreeSpot(g,p, "Place a " .. workerName(workerType)
                         , workerType , reg, q.next)
  end)

  local buildOn
  q.enQ(function ()
          buildOn = q.ans()
          doPlaceActive(g, buildOn, { owner = p, shape = workerType },q.next)
        end)
  q.enQ(function() noteBuiltOn(g,p,buildOn.edge); endAction(s,k) end)
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
      askFreeAdjacent(g,p,"Choose location",e,t,function(loc)
        doP(g,loc,{owner=p,shape=t},||placeCompWorkers(g,p,e,todo-1,k))
      end)
    end
  end

  if s.passive[trader] + s.passive[merchant] > 0 then
    askWorkerTypeOrPass(p,"Place passive?",s.passive,placing(doPlacePassive))

  elseif s.active[trader] + s.active[merchant] > 0 then
    askWorkerTypeOrPass(p,"Place active?",s.active,placing(doPlaceActive))

  else
    --XXX
    print(playerColorBB(p) .. " should be allowed to move a worker but this is not yet done.")
    k()
  end
end




function actReplaceOpponent(g,p,spots,k)
  print(playerColorBB(p) .. " chose to replace an opponent's worker.")

  local s = g.playerState[p]
  local loc
  local cost
  local oppS

  -- Choose who to bump
  local q = actQ()
  q.enQ(||askOccupiedSpot(p,"Replace",spots,q.next))
  q.enQ(function()
    loc = q.ans()
    w = loc.worker
    cost = 1
    if w.shape == merchant then cost = 2 end
    oppS = g.playerState[w.owner]
    q.next()
  end)

  -- They choose new location
  q.enQ(||askFreeAdjacent(g,w.owner,"New location?",loc.edge,w.shape,q.next))
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
      askWorkerType(p,"Place worker",s.active,q.next)
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
      q1.enQ(|| askWorkerType(p,"Deactivate worker",s.active,q1.next))
      q1.enQ(function()
        local t = q1.ans()
        doChangeActive(g,p,t,-1)
        doChangePassive(g,p,t,1)
        q1.next()
      end)
    end
    q1.enQ(q.next)
  end)

  q.enQ(||endAction(s,k))
end





--------------------------------------------------------------------------------
-- Move workers

function actMoveWorkers(g,p,ourSpots,k)
  print(playerColorBB(p) .. " chose to move workers.")
  local s = g.playerState[p]
  local maxMove = bookLevelMap[s.bookLevel]
  if #ourSpots < maxMove then maxMove = #ourSpots end

  local x = 2
  local y = 10
  local sem = newSem()

  local busy = flase
  local nextBtn = 1
  local funs = {}
  local function putDown(obj,me,spot) return function()
    if busy then
      print("Finish placing the other worker first.")
      return
    end
    busy = true
    local regs = {}
    regs[g.map.defaultRegion] = true
    regs[g.map.edges[spot.edge].region] = true
    askFreeSpot (g, p,"New location", spot.worker.shape,regs,function(newLoc)
      doPlaceWorker(g,newLoc,spot.worker,function()
        sem.down()
        busy = false
        obj.destroy()
        _G[funs[me]] = nil
      end)
    end)
  end
  end

  local function pickUp(spot,k)
    sem.up()
    doRemoveWorker(g,spot)

    spawnWorker(spot.worker, {x,2,y}, function(o)
      local fun = DYN_GLOB(putDown(o,nextBtn,spot))
      funs[nextBtn] = fun
      nextBtn = nextBtn + 1

      x = x + 2
      o.setScale({1,1,1})
      o.createButton({
        label = "∨",
        font_size = 800,
        width = 900,
        height = 900,
        rotation = { 0, 180, 0 },
        position = { 0, 1, 0 },
        color = playerColor(p),
        font_color = playerFontColor(p),
        click_function = fun
      })
      k()
    end)
  end

  local q = actQ()
  local curSpots = ourSpots
  local function rmSpot(sp)
    local new = {}
    for _,x in ipairs(curSpots) do
      if x.edge ~= sp.edge or x.stop ~= sp.stop then
        push(new,x)
      end
    end
    curSpots = new
  end

  local function choosePickUp()
    askOccupiedSpotOrPass(p,"Pickup worker",curSpots,function(i)
      if not i then q.stop(); sem.down()
               else pickUp(i,q.next)
      end
    end)
  end

  sem.up() -- waits to end picking things up
  for i = 1,maxMove do
    q.enQ(choosePickUp)
  end
  q.enQ(sem.down)

  sem.wait(||endAction(s,k))
end



--------------------------------------------------------------------------------
-- Complete a route

function actCompleteRoute(g,p,edges,k)
  local s = g.playerState[p]
  local q = actQ()
  local edge
  q.enQ(||askEdge(g, p, "Choose route", edges, q.next))
  q.enQ(function()
    edge = q.ans()
    print(playerColorBB(p) .. " completed a route between " ..
                        edge.from .. " and " .. edge.to)
    local c = getController(g,edge.from)
    if c then doScorePoints(g,c,1) end
    local c = getController(g,edge.to)
    if c then doScorePoints(g,c,1) end
    q.next()
  end)

  q.enQ(function()
    local opts = {}

    local function addOffice(n)
      local off = nextFreeOffice(g,n)
      if not off or off.level > s.buildingLevel then return end
      local have = false
      for _,stop in ipairs(edge.stops) do
        if stop.worker.shape == off.shape then have = true; break; end
      end
      if not have then return end
      push(opts, { text = "Build office in " .. n
                 , val  = ||doBuildOffice(g,n,edge.id,q.next)
                 })
    end

    addOffice(edge.from)
    addOffice(edge.to)
    -- XXX: add office using bonus
    -- do yellow action

    push(opts, { text = "Do nothing", val  = q.next })

    askText(p,"Choose action",opts,|f|f())

  end)

  -- Pick up bonus token.  We do this after resolving choices because
  -- it cannot be used during this action.
  q.enQ(function()
    if edge.bonus then doTakeBonus(g,p,edge.id,q.next) else q.next() end
  end)

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

  q.enQ(||endAction(s,k))
end


--------------------------------------------------------------------------------
-- Hire workers

function actHireWorkers(g,p,k)
  local s     = g.playerState[p]
  local limit = bagLevelMap[s.bagLevel]
  local ts    = s.passive[trader]
  local ms    = s.passive[merchant]
  if ts + ms <= limit then
    doChangePassive(g,p,trader,-ts)
    doChangeActive(g,p,trader,ts)
    doChangePassive(g,p,trader,-ms)
    doChangeActive(g,p,trader,ms)
    k()
    return
  end

  local q = actQ()

  local function hireOne()
    askWorkerType(p,"Hire worker",s.passive,function(t)
      doChangePassive(g,p,t,-1)
      doChangeActive(g,p,t,1)
      q.next()
    end)
  end

  for i = 1,limit do
    q.enQ(hireOne)
  end
  q.enQ(k)
end
