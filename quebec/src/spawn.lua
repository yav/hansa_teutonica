

function playerLeaderLoc(p)
  local ttsp = Player[p]
  local pos = ttsp.getHandTransform().position
  pos.x = pos.x - 3
  pos.z = pos.z + 1
  pos.y = 1.5
  return pos
end



function textColor(p)
  return playerFontColor(p)
end

function playerZoneLoc(p)
  return Player[p].getHandTransform().position
end

function powerZoneLoc(z)
  local x = zoneArrow[z]
  return { x[1], 3, x[2] }
end



function effect(o)
  local x0 = o.getRotation()[2]
  o.rotate({0,25,0})
  Wait.frames(|| o.rotate({0,-35,0}), 20)
  Wait.frames(|| o.rotate({0,10,0}), 40)
  Wait.frames(function ()
    local x = o.getRotation()[2]
    o.rotate({0,x0-x,0})
  end, 60)
end

function moveCubeEffect(p, p1, p2)
  spawnObject({
    type = "BlockSquare",
    position = p1,
    scale = { 0.75, 0.75, 0.75 },
    callback_function = function(o)
      o.setLock(true)
      o.setColorTint(playerColor(p))
      o.setPositionSmooth(p2, false, false)
      Wait.condition(|| Wait.frames(||o.destroy(),60), || o.resting)
    end
  })
end



function newGUI()
   local ui = {}
   ui.status = nil
   ui.player = {}
   ui.blueDiscs = {}
   ui.finished = {}
   ui.activeSites = {}   -- sites under construction, indexed by location
   ui.leaders = {}
   ui.baseEvent = nil
   return ui
end



function spawnBasics(k)

  local sem = newSem()

  sem.up()
  local b = spawnObject({
    type = "Custom_Token",
    position = { -4.5, 1, 3.5 },
    rotation = { 0, 180, 0 },
    scale = { 9, 1, 9 },
    callback_function = function(o)
      o.setLock(true)
      sem.down()
    end
  })
  b.setCustomObject({
    image = board_url
  })

  sem.up()
  GUI.status = spawnLabel(21,17,"Quebec",sem.down)

  sem.wait(k)
end

function setStatus(x)
  GUI.status.editButton({ index = 0, label = x })
end


function spawnUndoButton(p,k)
  spawnMenu(21,5,function(menu)
    spawnMenuItem(p,menu,0,"Start Over","undoTurn")
    GUI.undo = menu
    k()
  end)
end


function spawnAgeLabels(gs, k)
  for g,info in pairs(gs.buildingFor) do
    local t = spawnObject({
      type = "3DText",
      position = grid(g,1.2,0,0.5),
      rotation = { 90,0,0 }
    })
    t.setValue(info.age .. "")
  end
  k ()
end


function spawnBuildingSites(gs, k)
  local sem = newSem()
  for l,_ in pairs(gs.availableSites) do
    sem.up()
    spawnBuildingSite(l, sem.down)
  end

  for l,_ in pairs(gs.activeSites) do
    sem.up()
    spawnBuildingSite(l, sem.down)
  end

  sem.wait(k)
end



function spawnBuildingSite(l,k)
  local o = spawnObject({
    type = "Custom_Token",
    position = grid(l,1.3,0,0),
    scale = {0.75,0.75,0.75},
    rotation = { 0, 180, 0 },
    callback_function = function(o)
      o.setName("Available building site")
      o.setLock(true)
      GUI.blueDiscs[l] = o
      k()
    end
  })
  o.setCustomObject({
    image = site_url,
    type = 2,
    thickness = 0.1
  })
end


function spawnFinishedSite(gs,p,gloc,n,k)

  local b = spawnObject({
    type = "Custom_Token",
    position = grid(gloc,1.3,0,0),
    scale = {0.75,0.75,0.75},
    rotation = { 0,180,0},

    callback_function = function(o)
      o.setLock(true)
      GUI.finished[gloc] = o
      if n > 0 then
        o.createButton({
          font_size = 350,
          label = ownLabels[n],
          click_function = "nop",
          rotation = {0,0,0},
          color = playerColor(p),
          font_color = textColor(p),
          width = 800,
          height = 350,
          position = { 0,0.1,-1.5 }
        })
      end
      k()
    end
  })

  local info = gs.buildingFor[gloc]
  b.setCustomObject({
    type = 2,
    thickness = 0.1,
    image = build_url[info.type][info.age][info.ix]
  })
end



function spawnActiveSite(gs, l, k)
  local site = gs.activeSites[l]
  local p = site.owner
  local s = gs.playerState[p]
  local arch = nil
  if s.arch == l then arch = p end

  spawnArchitect(arch, l, function()
    local sem = newSem()
    for i,cp in ipairs(site.contributors) do
      sem.up()
      spawnBuilding(cp, l, i, sem.down)
    end
    sem.wait(k)
  end)
end

function spawnBuilding(p, loc, i, k)

  local a = i < 3 and -1 or 1
  local b = i == 1 and -1 or 1
  local l = grid(loc,1.8,a,b)

  labelledCube(p,l,hub[loc].requires, function(o)
    o.setName("Building contribution")
    GUI.activeSites[loc].contributors[i] = o
    k()
  end)

end


function spawnArchitect(p,loc,k)
  spawnObject({
    type = "PlayerPawn",
    position = grid(loc,1.4,1,-1),
    callback_function = function(o)
      o.setName("Architect")
      o.setLock(true)
      o.setColorTint(playerColor(p or "Yellow"))
      GUI.activeSites[loc] = { archObj = o, contributors = {} }
      k(o)
    end
  })
end


function spawnEvents(gs,k)
  local sem = newSem()
  local i = 0
  if (gs.baseEvent) then
    sem.up()
    spawnEvent(i,gs.baseEvent,true,gs.wonRace,sem.down)
    i = i + 1
  end
  if (gs.event) then
    sem.up()
    spawnEvent(i,gs.event,false,nil,sem.down)
  end
  sem.wait(k)
end


function spawnMarker(p,k)
  local pos = GUI.baseEvent.getPosition()
  GUI.wonRace = spawnObject({
    type = "PlayerPawn",
        position = { pos[1] - 2.5, pos[2], pos[3] - 1.5 },
        callback_function = function(o)
          o.setName("1st place")
          o.setLock(true)
          o.setColorTint(playerColor(p))
          k()
        end
      })
end

function spawnEvent(i,ev,base,marker,k)


  spawnMenu(21.5, -7*i, function(menu)
    menu.setName("Event")
    local lab = ""
      for i,t in ipairs(events[ev]) do
        local sep = (i == 1) and "" or "\n"
        lab = lab .. sep .. t
      end
      local bg = {0,0,0}
      menu.createButton(
        { font_size      = 300
        , font_color     = {1,1,1}
        , hover_color    = bg
        , press_color    = bg
        , color          = bg
        , label          = lab
        , click_function = "nop"
        , position       = { 0, 1.2, 0 }
        , rotation       = { 0, 180, 0 }
        , width          = 6000
        , height         = 400 * #events[ev]
        , tooltip        = "Event"
        }
      )

      if base then
        GUI.baseEvent = menu
      else
        GUI.event = menu
      end

      if marker then
        spawnMarker(marker,k)
      else
        k()
      end
    end)
end


function spawnLeaders(gs,k)
  local sem = newSem()
  for _,l in pairs(gs.leaders) do
    sem.up()
    spawnLeader(gs.currentAge, nil, l, sem.down)
  end
  sem.wait(k)
end



function spawnPortrait(age,l,pos,o,k)
  pos[3] = pos[3] + 2

  img = spawnObject({
    type = "Custom_Token",
    rotation = { 0, 180, 0 },
    position = pos,
    callback_function = function(me)
      o.addAttachment(me)
      k()
    end
  })
  img.setCustomObject({
   thickness = 0.1,
   image = leader_img_url[l][age]
  })
end

function spawnLeader(age,owner, l, k)
   local pos
   if not owner then
     pos = { -26, 2, 20 - l * 6 }
   else
     pos = playerLeaderLoc(owner)
   end

   obj = spawnObject({
     type = "Custom_Tile",
     rotation = { 0, 180, 0 },
     position = pos,
     callback_function = function(o)
       o.setLock(true)
       local n = leaders[l][age]
       o.setName(n .. "\n" .. zoneName[l] .. " Leader")
       o.setDescription(leaderD[l])
       spawnPortrait(age,l, pos,o,k)
     end
  })
  obj.setCustomObject({
    type = 3,
    thickness = 0.1,
    image = leader_url[l]
  })




  GUI.leaders[l] = obj
end





function spawnPlayers(gs,k)
  local sem = newSem()
  local j = gs.pnum < 5 and 1 or 0
  for i,p in ipairs(gs.players) do
    local ttsp = Player[p]
    ttsp.setHandTransform({
      position = {-33 + (j + i) * 11, -1, -17 },
      scale = { 6, 4.3, 6 },
    })
    ttsp.setCameraMode("TopDown")
    sem.up()
    spawnPlayer(gs,p,sem.down)
  end
  sem.wait(k)
end


function spawnPlayer(gs,p,k)

  -- UI stuff for this player goes here
   local ui = {}
   GUI.player[p] = ui

  local ttsp = Player[p]
  local l = ttsp.getHandTransform().position
  l.z = l.z + 2
  l.y = 1.5
  local i
  for j,q in ipairs(gs.players) do
    if p == q then i = j end
  end
  local dx = (i-1) % 3
  local dz = -math.floor((i-1)/3)


  -- zone UI objects go here
  local zoneObj = {}
  ui.zoneObj = zoneObj

  -- READ ONLY
  local s = gs.playerState[p]


  local todo = newSem()

  -- Spawn zone counters
  for z = 1,5 do
   local l = zoneLoc[z]
   local loc = { l[1] + dx, 2, l[2] + dz}
   todo.up()
   labelledCube(p,loc,s.zone[z], function(o)
     o.setName(zoneName[z] .. " influence")
     zoneObj[z] = o
     todo.down()
   end)
  end

  -- Spawn active workers
  todo.up()
  labelledCube(p, l, s.active, function(o)
    o.setName("Active workers")
    ui.activeObj = o
    todo.down()
  end)
  todo.up()
  ui.hammer = spawnObject({
    type = "Custom_Token",
    position = { l[1], l[2], l[3] - 1.25 },
    rotation = { 0, 180, 0 },
    scale = {0.3, 0.3, 0.3},
    callback_function = function(o)
      o.setLock(true)
      o.setColorTint(playerColor(p))
      o.setName("Active worklers")
      todo.down()
    end
  })
  ui.hammer.setCustomObject({
    image = hammer_url
  })

  -- Passive workers
  l.x = l.x + 2
  todo.up()
  labelledCube(p, l,s.passive, function(o)
    o.setName("Passive workers")
    ui.passiveObj = o
    todo.down()
  end)

  -- Score
  todo.up()
  scoreLab = spawnObject({
    type = "Custom_Token",
    position = { l[1] - 1.5, l[2], l[3] + 2 },
    scale = { 0.5, 0.5, 0.5 },
    rotation = { 0, 180, 0 },
    callback_function = function(o)
      o.setLock(true)
      o.setName("Vicotry points")
      ui.scoreLabel = o
      todo.down()
    end
  })
  scoreLab.setCustomObject({
    image = vp_tile_url
  })

  scoreText = spawnObject({
    type = "3DText",
    position = { l[1], l[2], l[3] + 2.4 },
    rotation = { 90,0,0 }
  })
  scoreText.setValue(s.score .. "")
  ui.scoreText = scoreText


  -- Architects
  if (s.arch) then
    todo.up()
    spawnActiveSite(gs, s.arch, todo.down)
  end

  if (s.arch2) then
    todo.up()
    spawnActiveSite(gs, s.arch2, todo.down)
  end

  for l,stars in pairs(s.owns) do
    todo.up()
    spawnFinishedSite(gs, p, l, stars, todo.down)
  end


  -- Leader
  if s.leader > 0 then
    todo.up()
    spawnLeader(gs.currentAge, p, s.leader, todo.down)
  end

  todo.wait(k)

end



function labelledCube(p,loc,amt,k)

  spawnObject({
    type = "BlockSquare",
    position = loc,
    scale = { 0.75, 0.75, 0.75 },
    callback_function = function(o)
      o.setLock(true)
      o.setColorTint(playerColor(p))
      o.createButton({
        font_size = 400,
        label = amt,
        click_function = "nop",
        rotation = {0,180,0},
        font_color = textColor(p),
        width = 0,
        height = 0,
        position = { 0,0.75,0 }
      })
      if k then k(o) end
    end
  })
end


