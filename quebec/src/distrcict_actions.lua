------------------------------------------------------------------------------
-- Actions form the districts

local actions = { {}, {}, {}, {} } -- code for district actions


function chooseDistrict(c)
  return function(gs,l)
    local opts = { }
    for i = 1,3 do
      opts[i] = spotByColor[c][i+1].id
    end
    askLocs(gsCurPlayer(gs), "District to Activate", opts,|i| spotByColor[c][i+1].act(gs,l))
  end
end




function sendNum(gs,p,q,num,k)
  local s = gs.playerState[p]
  local opts = { "Send 0" }
  local pa = { {0,0} }

  for i = 1,num do
    local j = i
    if j > s.passive then j = s.passive end
    local a = i - j
    if a > s.active then a = s.active end
    if (a + j) == i then
      pa[i + 1] = { j, a}
      local pass = (j > 0) and " " .. j or ""
      local extra = (a > 0) and (" " .. a .. " active") or ""
      local jn = (j > 0 and a > 0) and " and" or ""
      opts[i+1] = "Send" .. pass .. jn .. extra
    end
  end

  askText(p, q, opts, |i| k(pa[i][1],pa[i][2]))
end




-- The current player has the option to gain some influence
function addInfluence(gs,num,q,zs,k)
  local p = gsCurPlayer(gs)

  sendNum(gs, p, q, num, function(j,a)
    if j + a == 0 then
      k()
    else
      local function sendTo(z)
        local s = gs.playerState[p]
        setPassive(gs,p, s.passive - j)
        setActive(gs,p, s.active - a)
        addZone(gs,p, zs[z], a + j)
        moveCubeEffect(p, playerZoneLoc(p), powerZoneLoc(zs[z]))
        k()
      end

      if #zs == 1
        then sendTo(1)
        else askZone(p, "Influence Authority", zs, sendTo)
      end

    end
  end)
end


--------------------------------------------------------------------------------

actions[1][1] = chooseDistrict(1)

actions[1][2] = function(gs)
  local q = actQ()
  q.enQ(||addInfluence(gs, 1, "Citadelle",   {5}, q.next))
  q.enQ(||addInfluence(gs, 1, "Another Authority", {1,2,3,4}, ||nextTurn(gs)))
end

actions[1][3] = function(gs)
  local p = gsCurPlayer(gs)
  scorePoints(gs,p,1)
  activate(gs,p,1)
  addInfluence(gs, 1, "Any Authority",{1,2,3,4,5}, ||nextTurn(gs))
end


actions[1][4] = function(gs)
   local p = gsCurPlayer(gs)
   local k = || nextTurn(gs)
   local s = gs.playerState[p]

   local q = actQ()

   q.enQ(|| addInfluence(gs, 1, "Any Authority", {1,2,3,4,5}, q.next))

   local function maxInfluence()
     local n = 0
     for _,x in ipairs(s.zone) do
       if x > n then n = x end
     end
     return n
   end

   local moveNum
   q.enQ(function()
     local n = maxInfluence()
     if n == 0 then q.stop(); k (); return end
     if n > 2 then n = 2 end
     local opts = { "Move 0" }
     for i = 1,n do
       opts[i+1] = "Move " .. i
     end
     askText(p, "Choose:",opts, function(i)
       if i == 1 then q.stop(); k (); return end
       moveNum = i - 1
       q.next()
     end)
   end)

   local moveFrom
   q.enQ(function()
     local opts = {}
     for z,x in ipairs(s.zone) do
       if x >= moveNum then push(opts,z) end
     end
     askZone(p,"Move from", opts,function(fZ)
       moveFrom = opts[fZ]
       q.next()
    end)
  end)

  local moveTo
  q.enQ(function()
    local ix,to = 1,{}
    for toZ = 1,5 do
      if toZ ~= moveFrom then
        to[ix] = toZ
        ix = ix + 1
      end
    end
    askZone(p,"Move to", to,function(toZ)
      moveTo = to[toZ]
      q.next()
    end)
  end)

  q.enQ(function()
    addZone(gs,p,moveFrom, -moveNum)
    addZone(gs,p,moveTo, moveNum)
    moveCubeEffect(p,powerZoneLoc(moveFrom),powerZoneLoc(moveTo))
    k()
  end)

end


--------------------------------------------------------------------------------


actions[2][1] = chooseDistrict(2)
actions[2][2] = |gs| addInfluence(gs, 2,"Religion/Commerce", {1,3}, ||nextTurn(gs))
actions[2][3] = |gs| addInfluence(gs, 2,"Millitary/Culture", {2,4}, ||nextTurn(gs))
actions[2][4] = |gs| addInfluence(gs, 2,"Citadelle",         {5},   ||nextTurn(gs))


--------------------------------------------------------------------------------

actions[3][1] = chooseDistrict(3)
actions[3][2] = function(gs,loc)

  local p = gsCurPlayer(gs)
  local s = gs.playerState[p]
  local tot = s.passive + s.active
  local contr = {}
  for l,info in pairs(gs.activeSites) do
    if l ~= loc and #info.contributors < 3 and hub[l].requires <= tot then
      push(contr,l)
    end
  end

  if (#contr > 0) then
    askText(p, "Action:", {"Contribute", "Pass"}, function(i)
      if i == 1 then
        askLocs(p, "Building for Contribution", contr,function(i)
          local loc = contr[i]
          local need = hub[loc].requires
          local j = need
          if j > s.passive then j = s.passive end
          setPassive(gs,p, s.passive - j)
          need = need - j
          setActive(gs,p, s.active - need)
          placeBuilding(gs,p,loc,||nextTurn(gs))
        end)
      else
        nextTurn(gs)
      end
    end)

  else
    say("There is nowhere to contribute to.")
    nextTurn(gs)
  end

end

actions[3][3] = function(gs)
  local p = gsCurPlayer(gs)
  askText(p, "Action:", {"Start Building", "Pass"}, function(i)
    if i == 1 then startBuilding(gs) else nextTurn(gs) end
  end)
end

actions[3][4] = function(gs)
  activate(gs,gsCurPlayer(gs),3)
  nextTurn(gs)
end



--------------------------------------------------------------------------------

actions[4][1] = chooseDistrict(4)

actions[4][2] = function(gs)
  local p = gsCurPlayer(gs)
  local s = gs.playerState[p]
  if s.active >= 3 then scorePoints(gs,p,4)
  elseif s.active >= 2 then scorePoints(gs,p,3)
  elseif s.active >= 1 then scorePoints(gs,p,1)
  end
  nextTurn(gs)
end

actions[4][3] = function(gs)
  local p = gsCurPlayer(gs)
  local s = gs.playerState[p]
   local n = 0
   for i = 1,5 do
     if s.zone[i] > 0 then n = n + 1 end
   end
   if n >= 3 then scorePoints(gs,p, 4)
   elseif n == 2 then scorePoints(gs,p, 3)
   elseif n == 1 then scorePoints(gs,p, 1)
   end
   nextTurn(gs)
end

actions[4][4] = function(gs)
  local p = gsCurPlayer(gs)
  local s = gs.playerState[p]
  local opts = {}
  for l,stars in pairs(s.owns) do
    if stars < 3 then push(opts,l) end
  end

  if #opts == 0
  then
     nextTurn(gs)
  else
    askLocs(p, "Building to Upgrade", opts, function(i)
      bumpSite(gs,p,opts[i])
      nextTurn(gs)
    end)
  end

end




