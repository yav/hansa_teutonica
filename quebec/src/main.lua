-------------------------------------------------------------------------------
-- Turns and Rounds



function noGame()
  saveState = nil
  askText(nil, "Ready?", { "Start Without Events", "Start With Events" }, function(mode)
    local ps = {"Green","White","Pink","Orange","Brown"} -- XXX: DEVELOP
    -- ps = getSeatedPlayers() -- XXX: Develop
    if (#ps < 3) then
      say("The game requires at least 3 players.")
      noGame()
    else
      startNewGame(ps, mode)
    end
  end)
end

function finishedGame(gs)
  setStatus("Game Over")
  say("\nGame Over")
  local scores = {}
  for p,s in pairs(gs.playerState) do
    push(scores, { player = p, score = s.score })
  end
  table.sort(scores, |x,y| x.score > y.score)

  local place = 0
  local prevScore = 1000000
  local msg = {"Final Score:"}
  for _,s in ipairs(scores) do
    if s.score < prevScore then place = place + 1 end
    local txt =  place .. ". " .. playerColorBB(s.player) .. ": " .. s.score .. " VP"
    say(txt)
    push(msg,txt)
    prevScore = s.score
  end

  askTextMany(nil, msg, {}, ||0)

end


function startNewGame(ps, mode)
  saveState = nil
  local gs = gsNew()
  gs.mode = mode
  gsSetupPlayers(gs,shuffle(ps))
  gsSetupAges(gs)
  gs.curPlayer = 1
  gs.turn = 1

  finishGUI(gs, ||startAge(gs, function()
     saveGame(gs)
     takeTurn(gs)
  end))
end

-- Start playing either from a save state, or a new game
function finishGUI(gs, k)
  local q = actQ()

  local header = ageName(gs.currentAge)
  setStatus(header)

  q.enQ(|| spawnAgeLabels(gs,q.next))
  q.enQ(|| spawnBuildingSites(gs,q.next))
  q.enQ(|| spawnLeaders(gs,q.next))
  q.enQ(|| spawnEvents(gs,q.next))
  q.enQ(|| spawnPlayers(gs, q.next))
  q.enQ(k)
end



function startAge(gs, k)

  local sem = newSem()

  gs.currentAge = gs.currentAge + 1
  local nm = ageName(gs.currentAge)
  say(nm)

  setStatus(nm)

  gs.leaders = {1,2,3,4,5}
  sem.up()
  spawnLeaders(gs, sem.down)

  for i,l in ipairs(gs.ageLocs[gs.currentAge]) do
    -- if i > 1 then break end  -- XXX
     -- if math.floor(i % 3) == 1 then -- XXX
    sem.up()
    gs.availableSiteNum = gs.availableSiteNum + 1
    gs.availableSites[l] = true
    spawnBuildingSite(l, sem.down)
    -- end
  end

  if gs.mode == 2 then
    local age = gs.currentAge
    local opts = eventsByAge[age]
    if #opts > 0 then
      local n = math.random(#opts)
      -- n = 3 -- XXX
      local e = opts[n]
      if age == 1 then gs.baseEvent = e else gs.event = e end
      sem.up()
      spawnEvents(gs, function()
        check1867(gs)
        sem.down()
      end)
    end
  end

  sem.wait(k)
end


function check1867(gs)
  if gs.event ~= "e1867" then return end

  say("1867 Event")
  local scores = {}
  for p,s in pairs(gs.playerState) do
    push(scores, { player = p, score = s.score })
  end
  table.sort(scores, |x,y| x.score > y.score)
  local rank = 0
  local maxScore = 100000
  for _,p in ipairs(scores) do
    if p.score < maxScore then
      rank = rank + 1
      maxScore = p.score
    end
    activate(gs,p.player,rank)
  end

end



function endAge(gs, k)
  setStatus(gs.currentAge .. ". Ending " .. zoneName[gs.currentAge] .. " Age")
  say("\nEnding " .. zoneName[gs.currentAge] .. " Age")

  -- Finish Yellow Architect
  for _,p in ipairs(gs.players) do
    local s = gs.playerState[p]
    if s.arch2 then
      finishBuilding(gs,p, s.arch2, || reallyEndAge(gs,k))
      return
    end
  end

  reallyEndAge(gs,k)
end



function reallyEndAge(gs,k)

   -- destroy and return leaders
  for _,p in ipairs(gs.players) do
    local s = gs.playerState[p]
    s.leader = 0
    s.arch2 = nil
  end
  for l = 1,5 do
    GUI.leaders[l].destroy()
  end

  -- Sites that didn't get started are gone
  for l,_ in pairs(gs.availableSites) do
    GUI.blueDiscs[l].destroy()
    GUI.blueDiscs[l] = nil
  end
  gs.availableSiteNum = 0
  gs.availableSites = {}

  -- End of round scoring
  local winners = {}
  local prevZ

  local function scoreZ(z,k)
    prevZ = z
    say("\nScoring " .. zoneName[z] .. " influence")
    winners = {}
    local scores = {}
    local winScore = 0
    for _,p in ipairs(gs.players) do
      local s = gs.playerState[p]
      local n = s.zone[z]
      if n > winScore then
        winners = {p}
        winScore = n
      elseif n == winScore then
        push(winners,p)
      end
      scorePoints(gs,p,n)
      scores[p] = n
    end
    if gs.event == "e1759" and z == 5 then
      say("1759: Everyone cascades.")
      winners = gs.players
    end

    local msg = {zoneName[z] .. " Score"}
    for p,vp in pairs(scores) do
      push(msg, playerColorBB(p) .. ": " .. vp .. " VP")
    end

    askTextMany(gsCurPlayer(gs), msg, {"Continue"}, k)
  end

  local noCascade = {}
  if gs.event == "e2008" then
    for p,s in pairs(gs.playerState) do
      local count = 0
      for i = 1,4 do
        if s.zone[i] > 0 then count = count + 1 end
      end
      if count < 4 then
        say("\n2008: " .. playerColorBB(p) .. " may not cascade.")
        noCascade[p] = true
      end
    end
  end


  local function cascade(p, from, to)
    local s = gs.playerState[p]
    local have = s.zone[from]
    local n = math.floor(have / 2)
    if n == 0 then return end
    if n > 5 then n = 5 end

    local mayCascade = not noCascade[p]
    if gs.event == "e2001" and to and s.zone[to] == 0
       then say("\n2001: " .. playerColorBB(p) ..
                " may not cascade to " .. zoneName[to] .. ".")
            mayCascade = false
       end

    if mayCascade then
      setZone(gs,p,from,have - n)
      if to then
        setZone(gs,p,to,s.zone[to] + n)
        moveCubeEffect(p,powerZoneLoc(from),powerZoneLoc(to))
      else
        setActive(gs,p,s.active + n)
      end
    end
  end

  local function finishZone(from,to)
    for _,p in ipairs(winners) do
      cascade(p,from,to)
    end
    for _,p in ipairs(gs.players) do
      local s = gs.playerState[p]
      local have = s.zone[from]
      if have > 0 then
        setZone(gs,p,from,0)
        setPassive(gs,p,s.passive + have)
        moveCubeEffect(p,powerZoneLoc(from),playerZoneLoc(p))
      end
    end
  end

  local q = actQ()

  check1917(gs)
  check1955(gs)

  q.enQ(||scoreZ(5, q.next))

  for a = 0,3 do

    q.enQ(function()
      local next = gs.currentAge + a
      if next > 4 then next = next - 4 end
      finishZone(prevZ,next)
      scoreZ(next,q.next)
    end)
  end

  q.enQ(function()
    finishZone(prevZ,nil)
    if gs.event then
      check1871(gs)
      gs.event = nil
      GUI.event.destroy()
    end
    if gs.currentAge == finalAge then endGame(gs) else startAge(gs, k) end
  end)

end

function check1871(gs)
  if gs.event ~= "e1871" then return end

  for p,s in pairs(gs.playerState) do
    local a = s.active
    if a > 3 then
      say("\n1871: Active workers for " .. playerColorBB(p) .. " reduced to 3.")
      setActive(gs,p,3)
      setPassive(gs,p,s.passive + a - 3)
    end
  end
end

function check1955(gs)
  if gs.event ~= "e1955" then return end
  for p,s in pairs(gs.playerState) do
    local count = 0
    for z = 1,5 do
      if s.zone[z] > 0 then count = count + 1 end
    end
    if count == 5 then
      say("\n1955")
      scorePoints(gs,p,5)
    end
  end
end

function check1917(gs)
  if gs.event ~= "e1917" then return end
  for p,s in pairs(gs.playerState) do
    for z = 1,5 do
      local n = s.zone[z]
      if 1 <= n and n <= 2 then
        say("\n1917: " .. playerColorBB(p) .. " lost " .. zoneName[z] .. " influence.")
        setZone(gs,p,z,0)
        setPassive(gs,p,s.passive + n)
        moveCubeEffect(p, powerZoneLoc(z), playerZoneLoc(p))
      end
    end
  end
end


function endGame(gs)

  say("\nEnd Game Scoring")

  say("\nScoring Unfinished Buildings")
  for l,site in pairs(gs.activeSites) do
    local r = hub[l].requires
    for _,p in ipairs(site.contributors) do
      scorePoints(gs,p,r)
    end
  end

  say("\nScoring Active Workers")
  for p,s in pairs(gs.playerState) do
    scorePoints(gs,p,math.floor(s.active/2))
  end

  say("\nScoring Completed Buildings")
  local owned = {}
  for p,s in pairs(gs.playerState) do
    for l,_ in pairs(s.owns) do
      owned[l] = p
    end
  end

  local areas = findAreas(owned)
  for _,p in ipairs(gs.players) do
    if not areas[p] then areas[p] = { } end
  end

  for p,as in pairs(areas) do
    local largest = nil

    local stars = gs.playerState[p].owns

    local mainT = { 1,3,6 }
    local pts = 0
    local maxPtsSmall = 0
    local maxPtsBig = 0

    for r,a in pairs(as) do
      local v1 = 0
      local v2 = 0

      for _,l in ipairs(a) do
         local s = stars[l]
         v1 = v1 + s
         v2 = v2 + mainT[s]
      end
      if v2 > maxPtsBig
         then pts = pts + maxPtsSmall
              maxPtsSmall = v1
              maxPtsBig = v2
         else pts = pts + v1
      end
    end
    pts = pts + maxPtsBig
    scorePoints(gs,p,pts)
  end

  if gs.baseEvent == "e1603" then score1603(gs,areas) end

  gs.finished = true
  finishedGame(gs)
end



function score1603(gs,areas)
  say("\nScoring 1603 Event")
  local winners = {}
  local largestA = 1
  for p,as in pairs(areas) do
    local largestP = 0
    for _,a in pairs(as) do
      if #a > largestP then largestP = #a end
    end
    if largestP > largestA then
       largestA = largestP
       winners = {p}
    elseif largestP == largestA then
       push(winners,p)
    end
  end

  for _,p in ipairs(winners) do
    scorePoints(gs,p,8)
  end

end


function takeTurn(gs)
  if (gs.finished) then finishedGame(gs); return end

  local ready = false
  local p = gsCurPlayer(gs)
  local s = gs.playerState[p]

  if s.active == 0 and s.passive == 0 then
    endAge(gs,function()
      if gs.currentAge <= finalAge then takeTurn(gs) end
    end)
  else
     chooseAction(gs)
  end
end

function nextTurn(gs)
  local p = gsCurPlayer(gs)
  askText(p, playerColorBB(p) .. "' turn:", { "End Turn" }, function (i)
    GUI.undo.destroy()
    GUI.undo = nil
    gsNextPlayer(gs)
    gs.turn = gs.turn + 1
    saveGame(gs)
    takeTurn(gs)
  end)
end




function checkRaceEvents(gs,p)
  local owns = gs.playerState[p].owns

  local function event1665()
    local has = {}
    local count = 0
    for l,n in pairs(owns) do
      if n == 3 then
        local c = hub[l].color
        if not has[c] then has[c] = true; count = count + 1 end
      end
    end
    return count == 4
  end

  local function event1663()
    local has = {}
    local count = 0
    for l,_ in pairs(owns) do
      local c = hub[l].color
      if not has[c] then has[c] = 1
      elseif has[c] == 1 then has[c] = 2; count = count + 1
      end
    end
    return count >= 3
  end

  local function event1682()
    local count = 0
    for l,_ in pairs(owns) do
      if hub[l].color == 4 then count = count + 1 end
    end
    return count >= 3
  end

  if gs.wonRace == p then return end

  local success =
     (gs.baseEvent == "e1663") and event1663() or
     (gs.baseEvent == "e1665") and event1665() or
     (gs.baseEvent == "e1682") and event1682() or
     false

  if not success then return end

  if not gs.wonRace then
    gs.wonRace = p
    say(playerColorBB(p) .. " is 1st to achieved the objective")
    scorePoints(gs,p,8)
    spawnMarker(p,||1)
  else
    say(playerColorBB(p) .. " is 2nd to achieve the objective")
    scorePoints(gs,p,4)
    gs.baseEvent = nil
    gs.wonRace = nil
    GUI.baseEvent.destroy()
    GUI.baseEvent = nil
    if GUI.wonRace then GUI.wonRace.destroy(); GUI.wonRace = nil; end
  end

end









