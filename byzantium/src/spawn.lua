
function newGUI(g,k)
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
  -- if 1 == 1 then return end

  GUI = {}
  local q = actQ()

  q.enQ(||spawnBoard(q.next))
  q.enQ(||spawnCities(g,q.next))
  q.enQ(||spawnPlayers(g,q.next))
  q.enQ(k)
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
  sem.up(); spawnBlocker(24,4.3,15,23.5,sem.down)

  local x0 = -1.1
  local x = x0
  local y = 0.015
  local dx = 0.131
  local dy = 0.131
  local sz = 60

  spawnTip(x,y,sz, bulgars_1, bulgar_army_text);  x = x + dx
  spawnTip(x,y,sz, bulgars_2, bulgar_army_text);  x = x + dx
  spawnTip(x,y,sz, fortify_1, fortify_text);      x = x + dx
  spawnTip(x,y,sz, fortify_2, fortify_text);      x = x + dx

  x = x0; y = y + dy
  spawnTip(x,y,sz, byz_improve_1,  byz_improve_text);   x = x + dx
  spawnTip(x,y,sz, byz_improve_2,  byz_improve_text);   x = x + dx
  spawnTip(x,y,sz, emperor,        emperor_text);       x = x + dx
  spawnTip(x,y,sz, byz_civil_war,  byz_civil_war_text); x = x + dx
  spawnTip(x,y,sz, byz_fleet,      byz_fleet);          x = x + dx

  x = x0; y = y + dy
  spawnTip(x,y,sz, arab_improve_1,   arab_improve_text);    x = x + dx
  spawnTip(x,y,sz, arab_improve_2,   arab_improve_text);    x = x + dx
  spawnTip(x,y,sz, arab_improve_3,   arab_improve_text);    x = x + dx
  spawnTip(x,y,sz, caliph,           caliph_text);          x = x + dx
  spawnTip(x,y,sz, arab_civil_war_1, arab_civil_war_text);  x = x + dx
  spawnTip(x,y,sz, arab_civil_war_2, arab_civil_war_text);  x = x + dx
  spawnTip(x,y,sz, arab_fleet,       arab_fleet_text);      x = x + dx

  sem.wait(k)
end


function spawnTip(x,y,w,id,tip,k)
  local fg = Color(1,1,1,0)
  local bg = Color(0,0,0,0)
  GUI.board.createButton(
      { font_size      = 0
      , font_color     = fg
      , hover_color    = bg
      , press_color    = bg
      , color          = bg
      , label          = id
      , click_function = "nop"
      , position       = { x, 5, y }
      , rotation       = { 0, 0, 0 }
      , width          = w
      , height         = w
      , tooltip        = tip
      })
end

function spawnBlocker(x,y,w,h,k)
  return spawnObject(
    { type              = "BlockSquare"
    , position          = { x, 0.2, y }
    , scale             = { w, 0.2, h }
    , sound             = false
    , callback_function = function(o)
        o.setLock(true)
        o.setColorTint(Color(0.2,0.2,0.2))
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
  local x = -23.5 + dx * 10
  local y = -8 - dy * 5

  local h = 1
  local w = 1
  local sem = newSem()
  local function label(tip,msg)
    sem.up()
    spawnBox(x,y,Color(0.5,0.5,0.5),Color(0,0,0),tip,msg,sem.down)
  end
  local x0 = x
  label("Elite Army","E");  x = x + w
  label("Main Army","A");   x = x + w
  label("Levy","L");        x = x + w
  label("Movement","M");    x = x + 1.5 * w
  label("Treasury","$");    x = x + 1.5 * w
  label("VP","VP");         x = x + 1.5 * w

  x = x0; y = y - h

  local factions = {}
  ui.factions = factions
  sem.up()
  factions[byzantium] = spawnFaction(g,player,byzantium, x, y, sem.down)

  y = y - h
  sem.up()
  factions[arabs] = spawnFaction(g,player,arabs, x, y, sem.down)

  x = x0 + 0.5 * w
  y = y - h
  local p  = pstate.color
  local fg = playerFontColor(p)
  local bg = playerColor(p)
  local function info(id,tip,msg)
    sem.up()
    spawnBox(x,y,fg,bg,tip,msg,function(o)
      ui[id] = o
      sem.down()
    end)
  end

  -- ids should match the fields in the model
  info("available",      "Available",      pstate.available); x = x + w
  info("casualty",       "Casualty",       pstate.casualty);  x = x + 3.5 * w
  info("fortifications", "Fortifications", pstate.fortifications)

  y = y - h
  x = x0 + 0.5 * w
  label("Available","[00FF00]✓[-]"); x = x + w
  label("Casualty", "[FF0000]✗[-]"); x = x + 3.5 * w
  label("Fortifications", "F")

  sem.wait(k)
end


function spawnFaction(g,player,faction,x,y,k)
  local fg  = faction_fg_color[faction]
  local bg  = faction_bg_color[faction]
  local w   = 1

  local ui  = {}

  local pstate = getPlayerState(g,player)
  local f      = pstate.factions[faction]

  local sem = newSem()
  local function info(id,tip)
    sem.up()
    spawnBox(x,y,fg,bg,tip,factionValueLabel(id,f),function(o)
      ui[id] = o
      sem.down()
    end)
  end

  local x0 = x
  -- ids should match the names in the model
  info("eliteArmy", "Elite Army ($3)"); x = x + w
  info("mainArmy",  "Main Army ($1)");  x = x + w
  info("levy",      "Levy ($2)");       x = x + w
  info("movement",  "Movement ($1)");   x = x + 1.5 * w
  info("treasury",  "Treasury");        x = x + 1.5 * w
  info("vp",        "VP")

  ui.fieldArmy = nil
  if f.fieldArmy ~= nil then
    sem.up()
    spawnArmy(g,player,f.fieldArmy,function(o)
      ui.fieldArmy = o
      sem.down()
    end)
  end

  sem.wait(k)
  return ui
end

function factionValueLabel(stat,f)
  local lab = f[stat]
  if stat == "eliteArmy" and f.royalty then lab = lab .. "+1" end
  return lab
end


function spawnBox(x,y,fg,bg,tip,msg,k)
  spawnMenu(x,y,function(menu)
    menu.createButton(
      { font_size      = 300
      , font_color     = fg
      , hover_color    = bg
      , press_color    = bg
      , color          = bg
      , label          = msg
      , click_function = "nop"
      , position       = { 0, 5, 0 }
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
    local o = spawnObject({
      type = "Custom_Model",
      scale = { s,s,s },
      position = loc,
      sound = false,
      callback_function = spawned
    })
    o.setCustomObject({
      mesh = disc_url,
      material = 1
    })
  end
end


function redrawCity(g,name,k)
  GUI.cities[name].destroy()
  spawnCity(name,g.map.cities[name], k)
end
--------------------------------------------------------------------------------


function rollDice(player,n,k)
  local x = 20
  local y = 5
  local sem = newSem()
  local dice = {}
  for i = 1,n do
    sem.up()
    dice[i] = spawnObject(
      { type = "Die_6_Rounded"
      , sound = false
      , position = { x + i, 10, y }
      , callback_function = function(o)
          o.setColorTint(playerColor(player))
          o.roll()
          Wait.frames(||when(||o.resting,sem.down), 10)
        end
      }
    )
  end
  sem.wait(function()
    local result = {}
    for i,d in ipairs(dice) do
      result[i] = d.getRotationValue()
    end
    k(result)
  end)
end

