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

  GUI.officeWidth = {}
  GUI.officeWidth[trader] = 0.85
  GUI.officeWidth[merchant] = 0.95

  GUI.endGameInvest = {}

  local sem = newSem()
  sem.up(); spawnBoard(sem.down)
  sem.up(); spawnMap(g.map, sem.down)
  for _,p in pairs(g.players) do
    sem.up(); spawnPlayer(g,p,sem.down)
  end

  sem.wait(k)
end


function spawnBoard(k)
  GUI.board = spawnObject({
    type = "Custom_Tile",
    position = { boardDX, 1, boardDY },
    rotation = { 0, 180, 0 },
    scale = { 18, 1, 18 },
    callback_function = function(o)
      o.setLock(true)
      k()
    end
  })
  GUI.board.setCustomObject({
    image = board_url
  })
end

function spawnMap(m,k)
  local sem = newSem()
  for _,n in pairs(m.nodes) do
    sem.up()
    spawnNode(n,sem.down)
  end
  for _,e in ipairs(m.edges) do
    sem.up()
    spawnEdge(e,sem.down)
  end
  for i,w in pairs(m.endGameInvest) do
    sem.up()
    spawnInvest(m,w,i,sem.down)
  end
  sem.wait(k)
end

function investLocX(m,i) return m.investX + i * 1.2 end

function spawnInvest(m,w,i,k)
  GUI.endGameInvest[i] =
    spawnWorker(w,{investLocX(m,i),boardPieceZ,m.investY},k)
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
    x = x - w[worker.shape]
    sem.up()
    extra[i] = spawnWorker(worker,{x,boardPieceZ,y},sem.down)
  end
  ui.extraOffices = extra

  sem.wait(function()
    GUI.node[node.name] = ui
    k()
  end)
end


function spawnEdge(e,k)
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

  --[[
  -- XXX: DEVEL
  if not (e.x == nil) and not (e.y == nil) and not (e.rotation == nil) then
    e.bonus = 1
  end
  --]]

  if e.bonus and e.bonus < #bonus_token_url then
    sem.up()
    local loc = { e.x, boardPieceZ, e.y }
    ui.bonus = spawnBonus(e.bonus, loc, e.rotation, sem.down)
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
  if p == "Yellow" then return {0,0,0} else return {1,1,1} end
end

function playerColorBB(p)
  local c = playerColor(p)
  return string.format("[%02x%02x%02x]" .. p .. "[-]",
    math.floor(c[1] * 255),
    math.floor(c[2] * 255),
    math.floor(c[3] * 255))
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
  actDZ = 0.1
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
    spawnPlate(p,i,b,sem.down)
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

-- Bonus token in a player's area
function spawnPlate(g,p,ix,bonus,k)
  local loc = playerZoneLoc(g,p)
  ix = ix - 1
  local col = math.floor(ix / 4)
  local row = ix % 4
  loc[1] = loc[1] - 8 - col * 1.5
  loc[2] = 2
  loc[3] = loc[3] + 3 - row * 1.5
  spawnBonus(bonus,loc,180,function(o)
    push(GUI.player[p].plates,o)
    k(o)
  end)
end

function spawnBonus(bonus,loc,rot,k)
  local sc = 0.7
  local o = spawnObject(
    { type = "Custom_Model"
    , position = loc
    , rotation = { 0, rot, 0 }
    , scale = { sc, sc, sc }
    , callback_function = function(o)
        o.setLock(true)
        -- XXX: add description
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


