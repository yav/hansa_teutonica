function finalScoring(g)

  local score = {}
  local curStat = nil

  local function points(p,n)
    local v = score[p][curStat]
    score[p][curStat] = v + n
    doScorePoints(g,p,n)
  end

  local function newStat(p,x)
    curStat = x
    score[p][curStat] = 0
  end

  local rs = {}
  for _,r in ipairs(g.map.regions) do
    if r ~= g.map.defaultRegion then
      rs[r] = scoreRegion(g,r)
    end
  end


  for _,p in ipairs(g.players) do
    score[p] = {}
    local s = g.playerState[p]
    score[p]["game"] = s.score

    newStat(p,"upgrades")
    if s.actionLevel == #actionLevelMap     then points(p,4) end
    if s.bagLevel == #bagLevelMap           then points(p,4) end
    if s.buildingLevel == #buildingLevelMap then points(p,4) end
    if s.bookLevel == #bookLevelMap         then points(p,4) end

    newStat(p,"bonusMarkers")
    local n = totalBonus(g,p)
    if n > 0 then
      if n > #endGameBonusPoints then n = #endGameBonusPoints end
      points(p,endGameBonusPoints[n])
    end

    newStat(p,"invested")
    for i,w in ipairs(g.map.endGameInvest) do
      if w.owner == p then
        points(p,endGameInvestPoints[i])
      end
    end

    newStat(p,"control")
    for _,node in pairs(g.map.nodes) do
      if getController(g,node.name) == p then points(p,2) end
    end

    newStat(p,"keys")
    points(p, largestNetwork(g,p) * keyLevelMap[s.keyLevel])

    newStat(p,"regions")
    local total = 0
    for r,ps in pairs(rs) do
      total = total + ps[p]
    end
    points(p, total)

  end
end

function scoreRegion(g,r)
  local control = {}
  local offices = {}

  for _,p in ipairs(g.players) do
    control[p] = 0
    offices[p] = 0
  end

  for n,node in pairs(g.map.nodes) do
    local count = false
    for _,j in ipairs(node.region) do
      if r == j then count = true; break end
    end
    if count then
      local c = getController(g,n)
      if c then control[c] = control[c] + 1 end
      for _,p in ipairs(g.players) do
        offices[p] = offices[p] + getPresence(g,p,n)
      end
    end
  end

  local points  = {}
  local ps = {}
  for _,p in ipairs(g.players) do
    if offices[p] > 0 then push(ps,p) else points[p] = 0 end
  end
  if #ps == 0 then return points end

  table.sort(ps,|x,y| control[x] > control[y] or
                      control[x] == control[y] and offices[x] > offices[y])


  local rewards = {7,4,2,0,0}
  local prev    = ps[1]
  local group   = { ps[1] }
  local total   = rewards[1]

  local function finishGroup()
    local win = math.floor(total / #group)
    for _,q in ipairs(group) do points[q] = win end
  end

  for i = 2,#ps do
    local p = ps[i]
    if control[p] == control[prev] and offices[p] == offices[prev] then
      push(group,p)
      total = total + rewards[i]
    else
      finishGroup()
      prev = p
      group = {p}
      total = rewards[i]
    end
  end
  finishGroup()
  return points
end


