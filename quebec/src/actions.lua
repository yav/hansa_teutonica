-----------------------------------------------------------------------------
-- Player actions


function chooseAction(gs)

   local p = gsCurPlayer(gs)
   say("\nTurn #" .. gs.turn .. ": " .. playerColorBB(p) .. "'s turn")
   local s = gs.playerState[p]

   local acts = {}
   local actFun = {}
   local function addAct(txt,f)
     push(acts,txt)
     push(actFun, || spawnUndoButton(p,||f()))
   end

   addAct("Start Building", || startBuilding(gs))

   local contr = {}
   for l,info in pairs(gs.activeSites) do
     if #info.contributors < 3 and hub[l].requires <= s.active then
       push(contr,l)
     end
   end

   if #contr > 0 then
     addAct("Contribute", || contribute(gs,contr))
   end

   if s.active > 0 then
     addAct("Gain 1 Influence", || zoneWorker(gs))
   end

   if s.leader == 0 and gs.event ~= "e1763" then
     addAct("Take Leader", || takeLeader(gs))
   end

   askText(p, playerColorBB(p) .. "'s turn:", acts,|i| actFun[i]() )

end




function startBuilding(gs)
  local p = gsCurPlayer(gs)
  local s = gs.playerState[p]
  local gloc

  if s.leader == 3 then
    askText( p
           , "Which architect?"
           , { "Use " .. playerColorBB(p) .. " architect"
             , "Use [FFFF00]Yellow[-] architect"
            }, function(i)
        if i == 2 then
          say(playerColorBB(p) .. " uses the " .. zoneName[3] .. " leader.")
        end
        startBuildingWith(gs,p, i == 2)
    end)
  else
    startBuildingWith(gs,p,false)
  end
end



function startBuildingWith(gs,p,extra)
  local s = gs.playerState[p]

  local gloc
  if extra then gloc = s.arch2 else gloc = s.arch end

  local function oldSiteDone()
    local function finishTurn()
      if extra and s.leader ~= 3 then nextTurn(gs); return end -- round ended

      local sites = {}
      local function buildOn(i)
        local newLoc = sites[i]

        gs.availableSites[newLoc] = nil
        gs.availableSiteNum = gs.availableSiteNum - 1

        local newSite = { owner = p, contributors = {} }
        gs.activeSites[newLoc] = newSite
        if extra then s.arch2 = newLoc else s.arch = newLoc end


        say (playerColorBB(p) .. " started a building in the " ..
                                                        hub[newLoc].name)

        local col = p
        if extra then col = nil end
        spawnArchitect(col, newLoc,
          function()
            if gs.event == "e1812" then
              say("1812: activate 1 fewer worker.")
              activate(gs,p,2)
            elseif gs.event == "e1800" then
              say("1800---1830: activate 1 additional worker.")
              activate(gs,p,4)
            else
              activate(gs,p,3)
            end
            nextTurn(gs)
          end)
      end

      local ptrs = {}
      local ix = 1
      for l,_ in pairs(gs.availableSites) do
        local pos = grid(l,2,0,0)
        sites[ix] = l
        ptrs[ix] = { pos[1]+1.5, pos[3], -90 }
        ix = ix + 1
      end

      ask(p, "New Building Site", ptrs, buildOn)
    end

    if gs.availableSiteNum == 0 then
    endAge(gs,finishTurn)
    else
      finishTurn()
    end
  end

  if gloc then
     finishBuilding(gs,p,gloc, oldSiteDone)
  else
     oldSiteDone()
  end

end




function finishBuilding(gs,p,gloc, k)
  local s = gs.activeSites[gloc]
  local ui = GUI.activeSites[gloc]
  local stars = #s.contributors
  local district = hub[gloc]

  say(playerColorBB(p) .. " completed a " .. stars ..
                          " star building in the " .. district.name)

  local sem = newSem()
  local polAsked = false
  local polChosen = nil
  for ci,c in ipairs(s.contributors) do
    sem.up()
    local ps = gs.playerState[c]
    local tgtZ = district.color

    local function doPlace(tgtZ)
      ui.contributors[ci].destroy()
      ui.contributors[ci] = nil
      addZone(gs,c,tgtZ,district.requires)
      moveCubeEffect(c, grid(gloc,3,0,0), powerZoneLoc(tgtZ))
      sem.down()
    end

    if ps.leader == 2 then
      if polAsked
        then Wait.condition(||doPlace(polChosen), ||polChosen ~= nil)
        else
          polAsked = true
          say(playerColorBB(c) .. "uses " .. zoneName[2] .. " leader.")
          askZone(c,"Contributions from " .. district.name,{1,2,3,4},
            function(i)
              polChosen = i
              doPlace(i)
            end)
        end
    else
      doPlace(tgtZ)
    end
  end


  sem.wait(function()
    ui.archObj.destroy()
    GUI.activeSites[gloc] = nil
    GUI.blueDiscs[gloc].destroy()
    gs.activeSites[gloc] = nil
    markSite(gs,p,gloc,stars,k)
  end)

end




function contribute(gs,ls)
  cleanSave = false
  local p = gsCurPlayer(gs)
  askLocs(p, "Building for Contribution", ls, function(i)
    local gloc = ls[i]
    local s = gs.playerState[p]
    local district = hub[gloc]
    setActive(gs,p,s.active - district.requires)

    placeBuilding(gs,p, gloc, function()
      local owner = gs.activeSites[gloc].owner
      if s.leader == 1 or owner ~= p then
        if s.leader == 1 and owner == p then
          say(playerColorBB(p) .. " activates the " .. zoneName[1] .. " leader.")
        end
        district.act(gs,gloc)
      else
        nextTurn(gs)
      end
    end)
  end)
end


function zoneWorker(gs)
  cleanSave = false
  local p = gsCurPlayer(gs)
  local function doAdd(z)
    local s = gs.playerState[p]
    setActive(gs,p,s.active - 1)
    addZone(gs,p,z,1)
    moveCubeEffect(p,playerZoneLoc(p),powerZoneLoc(z))
    nextTurn(gs)
  end
  askZone(gsCurPlayer(gs), "Influence Authorithy", {1,2,3,4,5}, doAdd)
end


function takeLeader(gs)
  cleanSave = false
  local p = gsCurPlayer(gs)
  local avail = {}
  for _,l in pairs(gs.leaders) do
    push(avail,l)
  end
  askLeader(p,avail,function(i)
    local l = avail[i]
    say(playerColorBB(p) .. " took the " .. zoneName[l] .. " Leader.")
    if gs.event ~= "e1756" then
       activate(gs,p, 5 - #avail)
    else say("1756--1763: No worker activation")
    end

    gainLeader(gs,p,l)
    if l == 5 then
       sendNum(gs,p,"Influence Citadelle",3, function(pa,ac)
         local s = gs.playerState[p]
         setActive(gs,p,s.active-ac)
         setPassive(gs,p,s.passive-pa)
         addZone(gs,p,5,pa+ac)
         moveCubeEffect(p,playerZoneLoc(p),powerZoneLoc(5))
         nextTurn(gs)
       end)
    else
      nextTurn(gs)
    end
  end)
end



