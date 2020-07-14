----------------------
-- Common player actions

function gainLeader(gs,p,l)
  local s = gs.playerState[p]
  gs.leaders[l] = nil
  s.leader = l
  local obj = GUI.leaders[l]
  obj.setPositionSmooth(playerLeaderLoc(p),false,false)
end



function activate(gs,p,n)
  local s = gs.playerState[p]
  if s.passive < n then n = s.passive end
  say(playerColorBB(p) .. " activated " .. n .. " workers")
  setPassive(gs,p, s.passive - n)
  setActive(gs,p, s.active + n)
  return n
end

function addZone(gs,p,z,n)
  if n == 0 then return end
  local s = gs.playerState[p]
  setZone(gs,p,z,s.zone[z] + n)
  local event = n >= 0 and (" gained " .. n)
                      or (" lost " .. -n)
  say(playerColorBB(p) .. event .. " " .. zoneName[z] .. " influence")
end


function setActive(gs,p,n)
  local s = gs.playerState[p]
  local ui = GUI.player[p]
  s.active = n
  setLabel(ui.activeObj,n)
end

function setPassive(gs,p,n)
  local s = gs.playerState[p]
  local ui = GUI.player[p]
  s.passive = n
  setLabel(ui.passiveObj,n)
end

function setZone(gs,p,z,n)
  local s = gs.playerState[p]
  local ui = GUI.player[p]
  s.zone[z] = n
  setLabel(ui.zoneObj[z],n)
end



function scorePoints(gs,p,n)
  local s = gs.playerState[p]
  local ui = GUI.player[p]
  s.score = s.score + n
  ui.scoreText.setValue(s.score .. "")
  effect(ui.scoreLabel)
  say(playerColorBB(p) .. " scored " .. n .. " point"
                                          .. (n ~= 1 and "s" or ""))
end


function placeBuilding(gs,p,loc,k)
  local site = gs.activeSites[loc]
  push(site.contributors, p)
  local nth = #site.contributors
  spawnBuilding(p,loc,nth,function()
    moveCubeEffect(p,playerZoneLoc(p),grid(loc,3,0,0))
    say(playerColorBB(p) .. " contributed to "
         .. playerColorBB(site.owner) .. "'s building in the " ..
                      hub[loc].name)
    k()
  end)
end


function bumpSite(gs,p,gloc)
  local s = gs.playerState[p]
  local stars = s.owns[gloc] + 1
  s.owns[gloc] = stars
  setLabel(GUI.finished[gloc],ownLabels[stars])
end


function markSite(gs,p,gloc,n,k)
  if n == 0 then k(); return end
  if n == 1 and gs.event == "e1775" then
      say("1775: " .. playerColorBB(p) .. " does not get an ownership token.")
      k()
      return
  end

  local s = gs.playerState[p]
  local col = playerColor(p)
  local fcol = textColor(p)

  if s.leader == 4 then
    local pts = n
    if gs.pnum > 3 then pts = pts + 1 end
    say(playerColorBB(p) .. " used the " .. zoneName[4] .. " leader.")
    scorePoints(gs,p, pts)
  end

  s.owns[gloc] = n

  checkRaceEvents(gs,p)


  spawnFinishedSite(gs,p,gloc,n,k)
end


