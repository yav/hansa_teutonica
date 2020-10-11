
function newGUI(g,k)
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
  -- if 1 == 1 then return end

  addRuleSummary()

  GUI = {}

  local sem = newSem()
  local citiesDone = false
  sem.up(); spawnBoard(sem.down)
  sem.up(); spawnBulgarStats(g,sem.down)
  sem.up(); spawnActions(g,sem.down)
  sem.up(); spawnCities(g, function()
    citiesDone = true
    sem.down()
  end)
  sem.up(); when(||citiesDone, ||spawnPlayers(g,sem.down))
  sem.wait(k)
end



--------------------------------------------------------------------------------
function spawnBoard(k)
  local sem = newSem()
  sem.up()
  GUI.board = spawnObject({
    type = "Custom_Tile",
    position = { 0, 0, 0 },
    rotation = { 0, 180, 0 },
    scale = { 18, 1, 18 },
    callback_function = function(o)
      o.setLock(true)
      sem.down()
    end
  })
  GUI.board.setCustomObject({
    image = board_url
  })

  sem.up(); spawnBlocker(-15.7,-12.5,18,10.5,sem.down)
  sem.up(); spawnBlocker(24.5,4.3,18,23.5,sem.down)
  sem.wait(k)
end


function spawnBlocker(x,y,w,h,k)
  return spawnObject(
    { type              = "BlockSquare"
    , position          = { x, 0.2, y }
    , scale             = { w, 0.2, h }
    , sound             = false
    , callback_function = function(o)
        o.setLock(true)
        o.setColorTint(blocker_color)
        k(o)
      end
    }
  )
end
--------------------------------------------------------------------------------







--------------------------------------------------------------------------------
function spawnPlayers(g,k)
  GUI.players = {}

  local sem = newSem()
  for _,player in ipairs(g.players) do
    sem.up()
    spawnPlayer(g,player,sem.down)
  end
  sem.wait(k)
end

function spawnPlayer(g,player,k)
  local pstate = getPlayerState(g,player)
  local ui = {}
  GUI.players[player] = ui

  local o = pstate.order
  local dx = ({0,1,1,0})[o]
  local dy = ({0,0,1,1})[o]

  local x = 16.5 + dx * 9
  local y = 2    - dy * 5

  local h = 1
  local w = 1
  local sem = newSem()
  local function label(tip,msg,maint)
    if maint then tip = string.format("%s\nmaintenance: $%d",tip,maint) end
    sem.up()
    spawnBox(x,y,nil,Color(0.5,0.5,0.5),blocker_color,
                tip,msg,sem.down)
  end
  local x0 = x
  label(faction_stat_name.eliteArmy, "E",  3);   x = x + w
  label(faction_stat_name.mainArmy,  "A",  1);   x = x + w
  label(faction_stat_name.movement,  "M",  1);   x = x + w
  label(faction_stat_name.levy,      "L",  2);   x = x + 2   * w
  label(faction_stat_name.treasury,  "$",  nil); x = x + 1.5 * w
  label(faction_stat_name.vp,        "VP", nil); x = x + 1.5 * w

  x = x0; y = y - h

  local factions = {}
  ui.factions = factions
  sem.up()
  factions[byzantium] = spawnFaction(g,player,byzantium, x, y, sem.down)

  y = y - h
  sem.up()
  factions[arabs] = spawnFaction(g,player,arabs, x, y, sem.down)

  x = x0
  y = y - h
  local p  = pstate.color
  local fg = playerFontColor(p)
  local bg = playerColor(p)
  local function info(id,tip,msg)
    sem.up()
    if id == "fortifications" then
      local s = 0.5
      spawnDisc({x,0.3,y},s,function(o)
        o.setLock(true)
        o.setColorTint(playerColor(p))
        o.setName(tip)
        o.createButton(
          { font_size      = 300/s
          , font_color     = fg
          , hover_color    = bg
          , press_color    = bg
          , color          = bg
          , label          = msg
          , click_function = "nop"
          , position       = { 0, 0.5, 0 }
          , rotation       = { 0, 180, 0 }
          , width          = 0
          , height         = 0
          , tooltip        = tip
          })
        sem.down()
      end)
    else
      spawnBox(x,y,bg,fg,bg,tip,msg,function(o)
        ui[id] = o
        sem.down()
      end)
    end
  end

  -- ids should match the fields in the model
  label("Available","[00FF00]✓[-]");                          x = x + w
  info("available",      "Available",      pstate.available); x = x + 1.5 * w
  label("Casualty", "[FF0000]✗[-]");                          x = x + w
  info("casualty",       "Casualty",       pstate.casualty);  x = x + 2.5 * w
  info("fortifications", "Fortifications", pstate.fortifications)

  -- taxes
  sem.up()
  local taxBG = playerColor(player)
  local taxFG = playerFontColor(player)
  if pstate.taxed == 0 then
    taxBG.a = 0
    taxFG.a = 0
  end
  spawnBox( -23  +  1.5 * dx
          ,  5.5 + -1.5 * dy
          , taxBG
          , taxFG
          , taxBG
          , ""
          , pstate.taxed
          , function(o)
              o.setName(act_taxes_name)
              o.setDescription(act_taxes_text)
              ui.taxed = o
              sem.down()
            end
          )

  -- passed
  if pstate.passed > 0 then
    sem.up()
    local bg = playerColor(player)
    local fg = playerFontColor(player)
    spawnBox( 8.4 + 1.5 * pstate.passed
            , -12.3
            , bg, fg, bg
            , playerColorBB(player) .. " passed"
            , ""
            , function(o)
                ui.passed = o
                sem.down()
              end)
  end

  sem.wait(k)
end

function editTaxes(player,val)
  local fg = playerFontColor(player)
  local bg = playerColor(player)
  if val == 0 then fg = Color(0,0,0,0); bg = Color(0,0,0,0) end
  local ui = GUI.players[player].taxed
  ui.setColorTint(bg)
  ui.editButton(
    { index       = 0
    , label       = val
    , font_color  = fg
    , hover_color = bg
    , press_color = bg
    , color       = bg
    })
end



function spawnFaction(g,player,faction,x,y,k)
  local fg  = faction_fg_color[faction]
  local bg  = faction_bg_color[faction]
  local w   = 1

  local ui  = {}

  local pstate = getPlayerState(g,player)
  local f      = pstate.factions[faction]

  local sem = newSem()
  local function info(id)
    local tip = faction_stat_name[id]
    local c = playerColor(player)
    if id == "treasury" then
      c = nil
      tip = string.format("%s (%s)", tip, faction_currency[faction])
    elseif id == "vp" then
      c = nil
    end
    sem.up()
    spawnBox(x,y,c,fg,bg,tip,factionValueLabel(id,f),function(o)
      ui[id] = o
      sem.down()
    end)
  end

  local x0 = x
  -- ids should match the names in the model
  info("eliteArmy"); x = x + w
  info("mainArmy");  x = x + w
  info("movement");  x = x + w
  info("levy");      x = x + 2   * w
  info("treasury");  x = x + 1.5 * w
  info("vp")

  ui.fieldArmy = nil
  if f.fieldArmy ~= nil then
    sem.up()
    spawnArmy(g,player,f.fieldArmy,function(o)
      ui.fieldArmy = o
      sem.down()
    end)
  end

  -- XXX: spawn religion cubes

  sem.wait(k)
  return ui
end

function factionValueLabel(stat,f)
  local lab = f[stat]
  if stat == "eliteArmy" and f.royalty then lab = lab .. "+1" end
  return lab
end

function spawnBox(x,y,boxColor,fg,bg,tip,msg,k)
  local noBox = false
  local z     = 0.5
  if boxColor == nil then
    noBox = ture
    boxColor = Color(0,0,0,0)
    z = 0
  end
  spawnCube(boxColor, {x,y}, function(menu)
    menu.setName(tip)
    menu.createButton(
      { font_size      = 300
      , font_color     = fg
      , hover_color    = bg
      , press_color    = bg
      , color          = bg
      , label          = msg
      , click_function = "nop"
      , position       = { 0, z, 0 }
      , rotation       = { 0, 180, 0 }
      , width          = 500
      , height         = 500
      , tooltip        = tip
      }
    )
    k(menu)
  end)
end



function editBox(obj,val)
  obj.editButton({ index = 0, label = val })
end

function clickableBox(obj, suff, f)
  local btn = obj.getButtons()[1]
  local info = { label = btn.label, tip = btn.tooltip }
  local ui = GUI.clickableBox
  if ui == nil then ui = {}; GUI.clickableBox = ui end
  ui[obj.getGUID()] = info
  local newLab = info.label .. '?'
  local newTip = info.tip .. suff
  obj.editButton({ index = 0, click_function = f,
                  label = newLab, tooltip = newTip })
end

function notClickableBox(obj)
  local info = GUI.clickableBox[obj.getGUID()]
  obj.editButton({ index = 0, click_function = "nop"
                 , label = info.label, tooltip = info.tip })
end


--------------------------------------------------------------------------------

function spawnArmy(g,player,city,k)
  spawnObject(
    { type      = "PlayerPawn"
    , position  = armyPos(g,player,city)
    , sound     = false
    , callback_function = function(o)
        o.setLock(true)
        o.setColorTint(playerColor(player))
        k(o)
      end
    })
end

function armyPos(g,player,city)
  local n     = getPlayerState(g,player).order
  local angle = -math.pi * (1 - n / 6)
  local pos   = GUI.cities[city].getPosition()
  return { pos.x + 1.2 * math.cos(angle), 0.2, pos.z + 1.2 * math.sin(angle) }
end

--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
function spawnCities(g,k)
  GUI.cities = {}
  local sem = newSem()
  for name,city in pairs(g.map.cities) do
    sem.up()
    spawnCity(name,city,sem.down)
  end
  sem.wait(k)
end

function spawnCity(name,city,k)
  local loc = { city.x, 0.2, city.y }
  local s

  local function spawned(o)
    o.setLock(true)
    o.setName(name)
    local fact = city.faction
    local descr = "Faction: " .. faction_name[fact]
    if city.controlledBy ~= nil then
      descr = descr .. "\nControlled by "
                                .. playerColorBB(city.controlledBy) .. "."
    end
    descr = descr .. "\nStrength: " .. city.strength
    if city.fortified then
      descr = descr .. "\nFortified: +1 during siege."
    end

    if city.constantinople then
      descr = descr .. "\nThis city may not be claimed."
      descr = descr .. "\nSacking this city ends the game."
    end
    if city.bulgarStart then
      descr = descr .. "\nThe Bulgars may attack here."
    end
    if city.mediterranean then
      descr = descr .. "\nDirectly reachable from Constantinople."
    end



    o.setDescription(descr)

    o.setColorTint(faction_bg_color[city.faction])
    local fg = nil
    local bg = nil
    local msg = city.strength .. ""
    if city.fortified then msg = msg .. "+1" end

    local owner = city.controlledBy
    if owner ~= nil then
      fg = playerFontColor(owner)
      bg = playerColor(owner)
    else
      fg = faction_fg_color[city.faction]
      bg = faction_bg_color[city.faction]
    end

    local function scaled(x)
      return x/s
    end

    o.createButton(
      { font_size      = scaled(300)
      , font_color     = fg
      , hover_color    = bg
      , press_color    = bg
      , color          = bg
      , label          = msg
      , click_function = "nop"
      , position       = { 0, 0.5, 0 }
      , rotation       = { 0, 180, 0 }
      , width          = scaled(500)
      , height         = scaled(500)
      }
    )
    GUI.cities[name] = o
    k(o)
  end

  if city.constantinople then
    s = 1.5
    spawnObject(
      { type              = "BlockSquare"
      , position          = loc
      , scale             = { s, 1, s }
      , sound             = false
      , callback_function = spawned
      }
    )
  else
    s = 0.75
    spawnDisc(loc,s,spawned)
  end
end

function spawnDisc(loc,s,k)
  local o = spawnObject({
    type = "Custom_Model",
    scale = { s,s,s },
    position = loc,
    sound = false,
    callback_function = k
  })
  o.setCustomObject({
    mesh = disc_url,
    material = 1
  })
  return o
end


function redrawCity(g,name,k)
  GUI.cities[name].destroy()
  spawnCity(name,g.map.cities[name], k)
end
--------------------------------------------------------------------------------


-- XXX: can't see pips on arab dice
function rollDice(color,who,n,k)
  local x = 20
  local y = 9
  if who == defender then y = 5 end
  local sem = newSem()
  local dice = {}

  local ui = GUI.dice
  if ui == nil then ui = {}; GUI.dice = ui end
  ui[who] = dice

  for i = 1,n do
    sem.up()
    dice[i] = spawnObject(
      { type = "Die_6_Rounded"
      , sound = false
      , position = { x + i * 1.1, 10, y }
      , callback_function = function(o)
          o.setColorTint(color)
          o.roll()
          Wait.frames(||when(||o.resting,sem.down), 10)
        end
      }
    )
  end
  sem.wait(function()
    local hits = 0
    for i,d in ipairs(dice) do
      local v = d.getRotationValue()
      local hit = v >= 4
      local p = d.getPosition()
      if hit then
        p.z = p.z + 0.75
        hits = hits + 1
      else
        p.z = p.z - 0.75
        local c = Color(color)
        c.a = 0.2
        d.setColorTint(c)
      end
      d.setPositionSmooth(p,false,false)
    end
    k(hits)
  end)
end

function removeDice()
  local todo = GUI.dice
  if todo == nil then return end
  for _,ds in pairs(todo) do
    for _,d in ipairs(ds) do
      d.destroy()
    end
  end
  GUI.dice = nil
end

--------------------------------------------------------------------------------

function spawnActions(g,k)
  GUI.actions = {}
  local sem = newSem()
  for _,action in ipairs(allActionSpaces) do
    sem.up()
    spawnAction(action,g.actionSpaces[action],sem.down)
  end
  sem.wait(k)
end

function spawnAction(action,owner,k)
  local color = owner and playerColor(owner) or Color(0,0,0,0)
  spawnCube(color, actionLoc(action), function(o)
    o.setName(action_name[action])
    o.setDescription(action_text[action])
    GUI.actions[action] = o
    k(o)
  end)
end

function updateActionOwner(action,owner)
  local color = owner and playerColor(owner) or Color(0,0,0,0)
  GUI.actions[action].setColorTint(color)
end

function spawnCube(color,loc,k)
  local s = 1
  spawnObject(
    { type              = "BlockSquare"
    , position          = { loc[1], 0.5, loc[2] }
    , scale             = { s, s, s }
    , sound             = false
    , callback_function = function(o)
        o.setLock(true)
        o.setColorTint(color)
        k(o)
      end
    }
  )
end

function actionLoc(act)
  if act == taxes then
    return { -22.25, 2.7 }
  end

  local x0 = -23.44
  local y0 = -0.3
  local dx = 2.67
  local dy = 2.67
  local sx
  local sy
  if     act > 9 then sx = act - 10; sy = 2
  elseif act > 4 then sx = act - 5;  sy = 1
  else                sx = act - 1;  sy = 0
  end
  return { x0 + sx * dx, y0 - sy * dy }
end


function spawnBulgarStats(game,k)
  local x   = -24.2
  local y   = 8
  local fg  = faction_fg_color[bulgars]
  local bg  = faction_bg_color[bulgars]
  local tip = "Bulgar Army Strength"
  local msg = game.bulgarArmy .. ""
  spawnBox(x,y,bg,fg,bg,tip,msg,function(o)
    GUI.bulgarArmy = o
    k(o)
  end)
end


