function newGUI(g,k)
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end

  -- if 1 == 1 then return end

  GUI         = {}
  GUI.board   = nil
  GUI.node    = {}
  GUI.edge    = {}
  GUI.player  = {}

  GUI.fullCities = nil

  GUI.officeWidth = {}
  GUI.officeWidth[trader] = 0.85
  GUI.officeWidth[merchant] = 0.95

  GUI.endGameInvest = {}

  local sem = newSem()
  sem.up(); spawnBoard(g.map, sem.down)
  sem.up(); spawnMap(g.map, sem.down)
  for _,p in pairs(g.players) do
    sem.up(); spawnPlayer(g,p,sem.down)
  end

  spawnPlateCounter(g)

  sem.up(); spawnUndo(g,sem.down)
  sem.wait(k)
end

function spawnPlateCounter(g)
  local map = g.map
  local counter = map.counter
  local x = map.x + counter.x
  local y = map.y + counter.y

  GUI.platesBG = spawnBonusToken(bonusAct4,{x,boardPieceZ,y-0.4},{180,180,0},function(o)
    o.setName("Remaining bonus tokens")
    o.setRotation({180,0,0})
  end)

  GUI.plates = spawnObject(
    { type = "3DText"
    , position = { x, boardPieceZ+0.2, y }
    , rotation = { 90, 0, 0 }
    }
  )
  updatePlateCounter(g)
end

function updatePlateCounter(g)
  local n = #g.bonus - g.nextBonus + 1
  GUI.plates.setValue(n .. "")
  if n == 0 then
    local o = GUI.platesBG
    GUI.platesBG = nil
    o.destroy()
  end
end

function spawnUndo(g,k)
  local loc = undoLoc[g.map.orientation]
  GUI.undo = spawnMenu(loc[1],loc[2], function(m)
    spawnMenuItem(nil,m,0,"Undo","undoAction")
    k()
  end)
end




function spawnBoard(map,k)
  GUI.board = spawnObject({
    type = "Custom_Tile",
    position = { map.x, 1, map.y },
    rotation = { 0, 180, 0 },
    scale = { map.scale, 1, map.scale },
    callback_function = function(o)
      o.setLock(true)
      k()
    end
  })
  GUI.board.setCustomObject({
    image = map.url
  })
end

function fullCityLoc(m)
  local sc = 0.72
  local x = m.x + m.fullCitiesX + m.fullCities * sc
  local y = m.y + m.fullCitiesY
  return { x, boardPieceZ, y }
end

function spawnMap(m,k)
  local sem = newSem()

  sem.up()
  spawnObject(
    { type = "BlockSquare"
    , scale = { 0.5, 0.5, 0.5 }
    , position = fullCityLoc(m)
    , callback_function = function(o)
        GUI.fullCities = o
        o.setColorTint({0,0,0})
        o.setLock(true)
        o.setName(m.fullCities .. "")
        sem.down()
      end
    })

  for _,n in pairs(m.nodes) do
    sem.up()
    spawnNode(n,sem.down)
  end
  for _,e in ipairs(m.edges) do
    sem.up()
    spawnEdge(m,e,sem.down)
  end
  for i,w in pairs(m.endGameInvest) do
    sem.up()
    spawnInvest(m,w,i,sem.down)
  end
  sem.wait(k)
end

function investLocX(m,i) return m.x + m.investX + i * 1.2 end

function spawnInvest(m,w,i,k)
  GUI.endGameInvest[i] =
    spawnWorker(w,{investLocX(m,i),boardPieceZ,m.y + m.investY},function(o)
      o.setName(endGameInvestPoints[i] .. "")
      k()
      end)
end


function spawnNode(node,k)
  local x = node.x
  local y = node.y

  local ui = {}
  local sem = newSem()

  local offs = {}
  for i,off in ipairs(node.offices) do
    if not off.worker then break end
    sem.up()
    offs[i] = spawnWorker(off.worker,{x,boardPieceZ,y},sem.down)
    x = x + GUI.officeWidth[off.shape]
  end
  ui.offices = offs

  local x = node.x
  local extra = {}
  for i,worker in ipairs(node.extraOffices) do
    x = x - GUI.officeWidth[worker.shape]
    sem.up()
    extra[i] = spawnWorker(worker,{x,boardPieceZ,y},sem.down)
  end
  ui.extraOffices = extra

  sem.wait(function()
    GUI.node[node.name] = ui
    k()
  end)
end


function spawnEdge(m,e,k)
  local sem = newSem()
  local ui = {}
  ui.stops = {}
  for _,s in ipairs(e.stops) do
    if s.worker then
      sem.up()
      spawnWorker(s.worker,{s.x,boardPieceZ,s.y},function(o)
        ui.stops[s.id] = o
        sem.down()
      end)
    end
  end

  if e.bonus and e.bonus < #bonus_token_url then
    sem.up()
    local loc = { e.x, boardPieceZ, e.y }
    ui.bonus = spawnBonus(m, e.bonus, loc, e.rotation, sem.down)
  end


  sem.wait(function()
    GUI.edge[e.id] = ui
    k()
  end)
end


function playerZoneLoc(g,p)
  local n = g.playerState[p].turnOrder
  return { 20, 2, 20 - 7 * n }
end



function playerColor(p)
  return stringColorToRGB(p)
end

function playerFontColor(p)
  if p == "Yellow" or p == "White" then return {0,0,0} else return {1,1,1} end
end

function playerColorNote(p,txt)
  local c = playerColor(p)
  return string.format("[%02x%02x%02x]" .. txt .. "[-]",
    math.floor(c[1] * 255),
    math.floor(c[2] * 255),
    math.floor(c[3] * 255))
end

function playerColorBB(p)
  return playerColorNote(p,p)
end







function spawnPlayer(g,p,k)
  local s = g.playerState[p]

  local sem = newSem()
  local ui = {}
  local pos = playerZoneLoc(g,p)

  local actDX
  local actDY
  local actDZ = 0
  local function rel(x,z)
    return { pos[1] + actDX + x, pos[2] + actDZ, pos[3] + actDY }
  end

  -- The player board
  sem.up()
  ui.board = spawnObject({
    type = "Custom_Token",
    position = pos,
    scale = { 2, 1, 2 },
    rotation = { 0, 180, 0 },
    callback_function = function(o)
      o.setLock(true)
      sem.down()
    end
  })
  ui.board.setCustomObject({
    image = player_board_url[p]
  })

  -- Action Level Counter
  actDX = -0.5
  actDY = 1.3
  local step = 0.9
  ui.actionLevel = {}
  for i = s.actionLevel+1,#actionLevelMap do
    sem.up()
    spawnTrader(p,rel(step * i),true, function(o)
      o.setName("Take " .. actionLevelMap[i] .. " actions")
      ui.actionLevel[i] = o
      sem.down()
    end)
  end

  -- Bag Level Counter
  actDX = 1.75
  actDY = 0.4
  step = 0.9
  ui.bagLevel = {}
  for i = s.bagLevel+1,#bagLevelMap do
    sem.up()
    spawnTrader(p,rel(step*i),true,function(o)
      local num = bagLevelMap[i]
      if num == 50 then num = "all" end
      o.setName("Activate " .. num .. " workers")
      ui.bagLevel[i] = o
      sem.down()
    end)
  end

  -- Key Level Counter
  actDX = -6
  actDY = 1.70
  step = 0.8
  ui.keyLevel = {}
  for i = s.keyLevel+1,#keyLevelMap do
    sem.up()
    spawnTrader(p,rel(step*i),true,function(o)
      o.setName(keyLevelMap[i] .. " per office in largest network")
      ui.keyLevel[i] = o
      sem.down()
    end)
  end

  -- Building Level Counter
  actDX = -5.75
  actDY = -0.25
  step = 0.7
  ui.buildingLevel = {}
  for i = s.buildingLevel+1,#buildingLevelMap do
    sem.up()
    spawnTrader(p,rel(step*i),false,function(o)
      o.setName("Build " .. buildingLevelMap[i] .. " offices")
      ui.buildingLevel[i] = o
      sem.down()
    end)
  end


  -- Book Level Counter
  actDX = -2.4
  actDY = 0.15
  step = 0.95
  ui.bookLevel = {}
  for i = s.bookLevel+1,#bookLevelMap do
    sem.up()
    spawnMerchant(p,rel(step*i),function(o)
      o.setName("Move " .. bookLevelMap[i] .. " workers")
      ui.bookLevel[i] = o
      sem.down()
    end)
  end

  -- Active merchants
  actDX = -6
  actDY = 3

  sem.up()
  ui.active = {}
  spawnMerchant(p,rel(0),function(o)
    local sc = 0.65
    o.setScale({sc,sc,sc})
    o.setName("Active Merchants")
    addLabel(o, p, 600, s.active[merchant] .. "")
    ui.active[merchant] = o
    sem.down()
  end)

  -- Active traders
  sem.up()
  spawnTrader(p,rel(2),false,function(o)
    o.setScale ({1,1,1})
    addLabel(o, p, 300, s.active[trader] .. "")
    o.setName("Active traders")
    ui.active[trader] = o
    sem.down()
  end)

  -- Passive merchants
  actDX = 4
  sem.up()
  ui.passive = {}
  spawnMerchant(p,rel(0),function(o)
    local sc = 0.65
    o.setScale({sc,sc,sc})
    o.setName("Passive Merchants")
    addLabel(o, p, 600, s.passive[merchant] .. "")
    ui.passive[merchant] = o
    sem.down()
  end)

  -- Passive traders
  sem.up()
  spawnTrader(p,rel(2),false,function(o)
    o.setScale ({1,1,1})
    addLabel(o, p, 300, s.passive[trader] .. "")
    o.setName("Passive traders")
    ui.passive[trader] = o
    sem.down()
  end)

  -- Score
  actDX = 0
  actDY = 3.5
  ui.score = spawnObject(
    { type     = "3DText"
    , position = rel(0)
    , rotation = { 90, 0, 0 }
    })
  ui.score.TextTool.setFontColor(playerColor(p))
  ui.score.setValue(s.score .. " VP")

  -- Finished plates
  actDX = -0.9
  actDY = 1.6
  actDZ = 0.2
  ui.finishedPlates = spawnObject(
    { type     = "3DText"
    , position = rel(0)
    , rotation = { 90, 0, 0 }
    })
  ui.finishedPlates.TextTool.setFontColor(playerColor(p))
  ui.finishedPlates.setValue(s.finishedPlates .. "")

  ui.plates = {}
  for i,b in ipairs(s.plates) do
    sem.up()
    spawnPlate(g,p,i,b,function(o)
      push(ui.plates,o)
      sem.down()
    end)
  end

  sem.wait(function()
    GUI.player[p] = ui
    k()
  end)
end



function spawnWorker(w,loc,k)
  if w.shape == trader
    then return spawnTrader(w.owner,loc,false,k)
    else return spawnMerchant(w.owner,loc,k)
  end
end

function spawnTrader(p,loc,rotated,k)
  local s = 0.5
  return spawnObject({
    type = "BlockSquare",
    scale = { s,s,s },
    rotation = { 0, rotated and 45 or 0, 0 },
    position = loc,
    callback_function = function(o)
      o.setColorTint(playerColor(p))
      o.setLock(true)
      k(o)
    end
  })
end


function spawnMerchant(p,loc,k)
  local s = 0.35
  local o = spawnObject({
    type = "Custom_Model",
    scale = { s,s,s },
    position = loc,
    callback_function = function(o)
      o.setColorTint(playerColor(p))
      o.setLock(true)
      k(o)
    end
  })
  o.setCustomObject({
    mesh = round_token_model_url,
    material = 1
  })
  return o
end

-- Location of plate i for the given player
function plateLoc(g,p,ix)
  local loc = playerZoneLoc(g,p)
  ix = ix - 1
  local col = math.floor(ix / 4)
  local row = ix % 4
  loc[1] = loc[1] - 8 - col * 1.5
  loc[2] = boardPieceZ
  loc[3] = loc[3] + 3 - row * 1.5
  return loc
end

-- Bonus token in a player's area
function spawnPlate(g,p,ix,bonus,k)
  spawnBonus(g.map,bonus,plateLoc(g,p,ix),180,k)
end

function spawnBonus(m,bonus,loc,rot,k)
  return spawnBonusToken(bonus,loc,{0,rot,0},function(o)
    o.setName(bonusName(m,bonus))
    k(o)
  end)
end

function spawnBonusToken(bonus,loc,rot,k)
  local sc = 0.7
  local o = spawnObject(
    { type = "Custom_Model"
    , position = loc
    , rotation = rot
    , scale = { sc, sc, sc }
    , callback_function = function(o)
        o.setLock(true)
        k(o)
      end
    })
  o.setCustomObject(
    { mesh = bonus_token_model_url
    , diffuse = bonus_token_url[bonus]
    , material = 3
    })
  return o
end

function spawnPass(loc, k)
  local sc = 1
  local o = spawnObject(
    { type = "Custom_Tile"
    , position = loc
    , rotation = { 0, 180, 0 }
    , scale = { sc, sc, sc }
    , callback_function = function(o)
        o.setLock(true)
        k(o)
      end
    }
  )
  o.setCustomObject({ image = pass_url })
  return o
end

function effect(o)
end



function addLabel (o,p, sz, lab)
  o.createButton({
    font_size = sz,
    label = lab,
    click_function = "nop",
    rotation = {0,180,0},
    color = playerColor(p),
    font_color = playerFontColor(p),
    width = onClick and sz or 0,
    height = onClick and sz or 0,
    position = { 0,0.5,0 }
  })
end


function setLabel(x,l)
  for _,i in ipairs(x.getButtons()) do
    i.label = l
    x.editButton(i)
  end
  effect(x)
end

function spawnEndGame(g)
  spawnObject(
    { type = "BlockSquare"
    , position = { 3, 0, 15 }
    , callback_function = function(o)
        o.setLock(true)
        local function lab(w,a,c,l,x,y)
          o.createInput (
            { input_function = "nop"
            , position   = { x * 2, boardPieceZ, y * 0.9 }
            , rotation   = { 0, 180, 0 }
            , font_color = c
            , font_size  = 300
            , color      = { 0, 0, 0 }
            , value      = l
            , width      = w
            , height     = 400
            , alignment  = a
            })
        end
        local pnum = 1
        for p,stats in pairs(g.finalScore) do
          for i,stat in ipairs(stats) do
            if pnum == 1 then
              lab(2000, 2, {1,1,1}, stat.lab, 0, -i)
            end
            lab(1000, 4, playerColor(p), stat.val .. "", -(pnum+0.5), -i)
          end
          local i = #stats + 1.2
          if pnum == 1 then
            lab(2000, 2, {1,1,1}, "Total", 0, -i)
          end
          lab(1000, 4, playerColor(p), g.playerState[p].score.. "",
                                                      -(pnum+0.5), -i)

          pnum = pnum + 1
        end
      end
    }
  )
end



function spawnMenu(x,y,k)
  return spawnObject(
    { type              = "BlockSquare"
    , position          = { x, 0, y }
    , callback_function = function(o) o.setLock(true); k(o) end
    }
  )
end

function spawnMenuItem(p,menu,ix,lab,f)
  local bg  = f and {0,0,0} or {0.2,0.2,0.2}
  local fg  = {1,1,1}
  local msg = lab
  if p and f then
    msg = playerColorNote(p, "> ") .. lab .. playerColorNote(p, " <")
  end
  menu.createButton(
    { font_size      = 300
    , font_color     = fg
    , color          = bg
    , label          = msg
    , click_function = f or "nop"
    , position       = { 0, boardPieceZ, -ix }
    , rotation       = { 0, 180, 0 }
    , width          = 4000
    , height         = 400
    }
  )
end


