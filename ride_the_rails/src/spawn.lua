function newGUI(g,k)

  for _,o in pairs(getAllObjects()) do
    o.destroy()
  end

  GUI =
    { turnOrder = {}
    , players   = {}
    , trains    = {}
    , map       = locMapEmpty()
    , traveller = nil
    }

  local sem = newSem()
  for l,spot in locsIn(g.map.locations) do
    sem.up(); spawnHex(spot,gridToWorld(l,board_z),sem.down)
    local ui = { trains = {}, passenger = nil }
    locMapInsert(GUI.map, l, ui)
    if spot.passenger then
      sem.up(); spawnPassengerAt(l,sem.down)
    end
    local q = actQ()
    sem.up()
    for c,_ in pairs(spot.trains) do
      q.enQ(||spawnTrainAt(l,c,q.next))
    end
    q.enQ(sem.down)
  end
  sem.up(); spawnTurnOrder(g,sem.down)

  for p,s in pairs(g.playerState) do
    sem.up(); spawnPlayer(s,sem.down)
  end

  sem.up(); spawnTrainSupply(g,sem.down)

  sem.wait(k)
end

function gridToWorld(loc,z)
  local side = 1
  local w    = 2   * side * math.cos(math.pi/6)
  local h    = 1.5 * side
  local r,c  = loc.row, loc.col
  local x    = (c + r/2) * w
  local y    = r * h
  return Vector(x,z,y)
end



--------------------------------------------------------------------------------


function spawnTurnOrder(g,k)
  local sem = newSem()
  for i,p in ipairs(g.turnOrder) do
    local loc = Vector(turn_order_x + 1.2 * (i-1), disc_z, turn_order_y)
    sem.up(); spawnDisc(p,loc,function(o)
      GUI.turnOrder[i] = o
      sem.down()
    end)
  end
  sem.wait(k)
end


function playerZone(s)
  local x = player_x + 10 * (s.id - 1)
  local y = player_y
  return x,y
end

function spawnPlayer(s,k)

  local p = s.color
  local x,y = playerZone(s)
  local ui = {}
  GUI.players[p] = ui

  local sem = newSem()
  sem.up()
  spawnMenu(x,y,function(o)
    local bg = playerColor(p)
    local fg = playerFontColor(p)
    ui.money = o
    o.createButton(
      { label           = ""
      , color           = bg
      , hover_color     = bg
      , press_color     = bg
      , font_color      = fg
      , rotation        = { 0, 180, 0 }
      , position        = { 0, txt_question_z, 0 }
      , font_size       = 300
      , width           = 800
      , height          = 700
      , tooltip         = playerColorBB(p) .. " money"
      , click_function  = "nop"
      })
    editMoney(p,s.money)
    sem.down()
  end)

  ui.trains = {}
  for co,n in pairs(s.shares) do
    ui.trains[co] = {}
    for i = 1,n do
      sem.up()
      spawnShare(s,co,i,sem.down)
    end
  end
  sem.wait(k)
end




function editMoney(p,x)
  GUI.players[p].money.editButton({ index = 0, label = string.format("$%d",x) })
end

function spawnShare(s,company,n,k)
  local x,y = playerZone(s)
  local cix
  for i,c in ipairs(companyName) do
    if company == c then cix = i; break end
  end
  local loc = Vector(x + (cix-1) * 1.5, train_z, y - 1 - n)
  spawnTrain(company,loc,function(o)
    GUI.players[s.color].trains[company][cix] = o
    k()
  end)
end





function spawnTrainSupply(g,k)
  local trains = {}
  GUI.trains = trains
  local sem = newSem()
  local i = 0
  for co,n in pairs(g.supply) do
    local color = companyColor(co)
    local me    = colorNote(color,co)
    sem.up()
    local loc = Vector(supply_x, train_z, supply_y - i)
    spawnTrain(co,loc,function(o)
      o.setName(string.format("%s from round %d", me, companyRoundStart[co]))
      trains[co] = o
      local fg = Color(1,1,1)
      local bg = Color(0,0,0)
      o.createButton(
        { label           = ""
        , color           = bg
        , hover_color     = bg
        , press_color     = bg
        , font_color      = fg
        , rotation        = { 0, 180, 0 }
        , position        = { 3, 0, 0 }
        , font_size       = 300
        , width           = 800
        , height          = 700
        , tooltip         = "Remaining " .. me .. " trains"
        , click_function  = "nop"
        })
      editTrainSupply(co,n)
      sem.down()
    end)
    i = i + 1
  end
  sem.wait(k)
end

function editTrainSupply(co,n)
  GUI.trains[co].editButton({index=0,label=n .. ""})
end


function spawnPassengerAt(loc,k)
  local pos = gridToWorld(loc,meeple_z)
  pos.z = pos.z + 0.7
  spawnMeeple(pos,function(o)
    locMapLookup(GUI.map,loc).passenger = o
    k(o)
  end)
end


function spawnTrainAt(loc,company,k)
  local pos    = gridToWorld(loc,train_z + 0.2)
  spawnTrain(company,pos,function(o)
    local ui     = locMapLookup(GUI.map,loc)
    local others = ui.trains
    push(others,o)
    local sc = 1.1 - 0.12 * #others
    local basex = 0
    if #others > 3 then basex = -0.25 end
    for i,t in ipairs(others) do
      t.setScale({sc,sc,sc})
      local x = basex + pos.x + 1.7 * sc * math.floor((i-1) / 3)
      local z = pos.z - 0.5 * sc * ((i - 1) % 3)
      t.setPosition(Vector(x, pos.y, z))
    end
    k(o)
  end)
end




--------------------------------------------------------------------------------
-- Just the shapes

function spawnTrain(companyName, loc, k)
  local s = 0.75
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , scale        = {s,s,s}
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        local c = companyColor(companyName)
        o.setColorTint(c)
        o.setName(string.format("%s train", colorNote(c,companyName)))
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = train_url
    , material = 1
    }
  )

  return o
end


function spawnMeeple(loc, k)
  local s = 1
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , scale        = {s,s,s}
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        o.setName("Passenger")
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = meeple_url
    , material = 1
    }
  )

  return o
end


function spawnDisc(p, loc, k)
  local s = 0.5
  local o = spawnObject(
    { type         = "Custom_Model"
    , position     = loc
    , scale        = {s,s,s}
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        local c = playerColor(p)
        o.setColorTint(c)
        o.setName(string.format("%s player", playerColorBB(p)))
        k(o)
      end
    }
  )
  o.setCustomObject(
    { mesh     = disc_url
    , material = 1
    }
  )

  return o
end

function spawnHex(spot, loc, k)
  local s = 0.5
  if spot.url == nil then k() end
  local o = spawnObject(
    { type         = "Custom_Token"
    , position     = loc
    , rotation     = { 0, 180, 0 }
    , scale        = {s,s,s}
    , sound        = false
    , callback_function = function(o)
        o.setLock(true)
        local lab = "Plains"
        if spot.terrain == terrainCity then
          lab = "City"
          if spot.bonus > 0 then
            lab = string.format("%s %s$%d"
                               , lab
                               , spot.bonusType == bonusRecurring and "∞ " or ""
                               , spot.bonus )
          end
        elseif spot.terrain == terrainMountains then lab = "Mountains"
        end
       o.setName(lab)
        k(o)
      end
    }
  )
  local url
  o.setCustomObject(
    { image = spot.url
    }
  )

  return o
end

