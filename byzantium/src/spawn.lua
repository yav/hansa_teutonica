local GUI

function newGUI(g,k)
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
  -- if 1 == 1 then return end

  GUI = {}
  local sem = newSem()

  sem.up(); spawnBoard(sem.down)
  sem.up(); spawnPlayers(g,sem.down)
  sem.up(); spawnCities(g,sem.down)

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
  sem.up(); spawnBlocker(24,4.3,15,23.5,sem.down)

  local x0 = -1.1
  local x = x0
  local y = 0.015
  local dx = 0.131
  local dy = 0.131
  local sz = 60

  local msg = table.concat(
    { "Bulgar Army (Byzantium/Arabs)"
    , ""
    , "1. Increase Bulgar army strength by 2"
    , "2. Either:"
    , "     - Attack neighbouring city, or"
    , "     - Increase strength by 2 more."
    , ""
    , "* Strength may not exceed 11."
    , "* May only use roads."
    },"\n")
  spawnTip(x,y,sz,"bulgar-1",msg); x = x + dx
  spawnTip(x,y,sz,"bulgar-2",msg); x = x + dx

  msg = table.concat(
    { "Fortify City (Byzantium/Arabs)"
    , ""
    , "Add one of your fortifications"
    , "to a city you control."
    , ""
    , "* Fortifications add +1 when"
    , "  a city is besieged."
    , ""
    , "* Fortifications stay with the city"
    , "  until it is sacked."
    , ""
    , "* A city may have only one"
    , "  foritification."
    }, "\n")
  spawnTip(x,y,sz,"fortify-1",msg); x = x + dx
  spawnTip(x,y,sz,"fortify-2",msg); x = x + dx

  x = x0; y = y + dy

  msg = table.concat(
    { "Improve City (Byzantium)"
    , ""
    , "Add +1 strength to a Byzantine city."
    , ""
    , "* No need to control the city."
    , ""
    , "* City strength may not exceed 3"
    , "  (fortifications do not count)."
    }, "\n")
  spawnTip(x,y,sz,"byz-improve-1",msg); x = x + dx
  spawnTip(x,y,sz,"byz-improve-2",msg); x = x + dx

  msg = table.concat(
    { "Emperor (Byzantium)"
    }, "\n")
  spawnTip(x,y,sz,"emperor",msg); x = x + dx

  msg = table.concat(
    { "Civil War (Byzantium)"
    }, "\n")
  spawnTip(x,y,sz,"byz-civil-war",msg); x = x + dx

  msg = table.concat(
    { "Fleet (Byzantium)"
    }, "\n")
  spawnTip(x,y,sz,"byz-fleet",msg); x = x + dx

  x = x0; y = y + dy

  msg = table.concat(
    { "Improve City (Arab)"
    , ""
    , "Add +1 strength to an Arab city."
    , ""
    , "* No need to control the city."
    , ""
    , "* City strength may not exceed 3"
    , "  (fortifications do not count)."
    }, "\n")
  spawnTip(x,y,sz,"arab-improve-1",msg); x = x + dx
  spawnTip(x,y,sz,"arab-improve-2",msg); x = x + dx
  spawnTip(x,y,sz,"arab-improve-3",msg); x = x + dx

  msg = table.concat(
    { "Caliph (Arab)"
    }, "\n")
  spawnTip(x,y,sz,"caliph",msg); x = x + dx

  msg = table.concat(
    { "Civil War (Arab)"
    }, "\n")
  spawnTip(x,y,sz,"arab-civil-war-1",msg); x = x + dx
  spawnTip(x,y,sz,"arab-civil-war-2",msg); x = x + dx

  msg = table.concat(
    { "Fleet (Arab)"
    }, "\n")
  spawnTip(x,y,sz,"arab-fleet",msg); x = x + dx

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
        o.setColorTint(Color(0,0,0))
        k(o)
      end
    }
  )
end
--------------------------------------------------------------------------------







--------------------------------------------------------------------------------
function spawnPlayers(g,k)
  local sem = newSem()
  for _,pstate in pairs(g.playerState) do
    sem.up()
    spawnPlayer(pstate,sem.down)
  end
  sem.wait(k)
end

function spawnPlayer(pstate,k)
  local o = pstate.order
  local dx = ({0,1,1,0})[o]
  local dy = ({0,0,1,1})[o]
  local x = -23.5 + dx * 10
  local y = -8 + dy *-5

  local h = 1
  local w = 1
  local sem = newSem()
  local function label(tip,msg)
    sem.up()
    spawnBox(x,y,Color(0.5,0.5,0.5),Color(0,0,0),tip,msg,sem.down)
  end
  local x0 = x
  label("Elite Army","E"); x = x + w
  label("Main Arymy","A"); x = x + w
  label("Levy","L"); x = x + w
  label("Movement","M"); x = x + 1.5 * w
  label("Treasury","$"); x = x + 1.5 * w
  label("VP","VP"); x = x + 1.5 * w

  x = x0
  y = y - h
  sem.up(); spawnFaction(x,y,byzantium, pstate.factions[byzantium],sem.down)
  y = y - h
  sem.up(); spawnFaction(x,y,arabs, pstate.factions[arabs],sem.down)
  y = y - h
  local p  = pstate.color
  local fg = playerFontColor(p)
  local bg = playerColor(p)
  local function info(tip,msg)
    sem.up()
    spawnBox(x,y,fg,bg,tip,msg,sem.down)
  end
  info("Available",pstate.available); x = x + w
  info("Casualty",pstate.casualty);   x = x + 2 + 1.5*w
  info("Fortifications",pstate.fortifications)

  y = y - h
  x = x0
  label("Available","[00FF00]✓[-]"); x = x + w
  label("Casualty", "[FF0000]✗[-]"); x = x + 3.5 * w
  label("Fortifications", "F")

  sem.wait(k)
end


function spawnFaction(x,y,name,f,k)
  local fg  = faction_fg_color[name]
  local bg  = faction_bg_color[name]
  local w   = 1
  local sem = newSem()
  local function info(tip,msg)
    sem.up()
    spawnBox(x,y,fg,bg,tip,msg,sem.down)
  end

  local x0 = x
  info("Elite Army ($3)",f.eliteArmy); x = x + w
  info("Main Army ($1)",f.mainArmy); x = x + w
  info("Levy ($2)",f.levy); x = x + w
  info("Movement ($1)",f.movement); x = x + 1.5 * w
  info("Treasury", f.treasury); x = x + 1.5 * w
  info("VP", f.vp)
  sem.wait(k)
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
  local o = GUI.cities[name]
  o.destroy()
  spawnCity(name,g.map.cities[name],k)
end
--------------------------------------------------------------------------------



