local GUI

function newGUI(g,k)
  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end
  -- if 1 == 1 then return end

  GUI = {}
  local sem = newSem()

  sem.up(); spawnBoard(sem.down)
  -- sem.up(); spawnDisc({1,0,0,{00,1.2,0},sem.down)
  sem.up(); spawnPlayer(0,0,newPlayer(),sem.down)

  sem.wait(k)
end

function spawnBoard(k)
  GUI.board = spawnObject({
    type = "Custom_Tile",
    position = { -2.5, 1, 0 },
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


function spawnDisc(color,loc,k)
  local s = 0.75
  local o = spawnObject({
    type = "Custom_Model",
    scale = { s,s,s },
    position = loc,
    callback_function = function(o)
      o.setColorTint(color)
      o.setLock(true)
      k(o)
    end
  })
  o.setCustomObject({
    mesh = disc_url,
    material = 1
  })
  return o
end



function spawnPlayer(x,y,pstate,k)
  local h = 1
  local sem = newSem()
  sem.up(); spawnFaction(x,y,byzantium_fg_color,byzantium_bg_color,
                                          pstate.factions[byzantium],sem.down)
  y = y - h
  sem.up(); spawnFaction(x,y,arabs_fg_color, arabs_bg_color,
                                          pstate.factions[arabs],sem.down)

  sem.wait(k)
end


function spawnFaction(x,y,fg,bg,f,k)
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
  info("Treasury", f.treasury); x = x + 1
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
      , position       = { 0, 1.2, 0 }
      , rotation       = { 0, 180, 0 }
      , width          = 500
      , height         = 500
      , tooltip        = tip
      }
    )
  end)
end

