

function askInvestmentSpot(m,p,spots,k)
  if #spots == 1 then k(spots[1]); return end
  local opts = {}
  for i,x in ipairs(spots) do
    opts[i] = { x = investLocX(m,x), y = m.investY, r = 0, val = x }
  end
  ask(p, ptrWorker(p, {owner=p,shape=merchant},'?'), "Choose spot", opts, k)
end


function askWorkerType(p,q,workers,k)
  if workers[trader] == 0 then k(merchant); return end
  if workers[merchant] == 0 then k(trader); return end

  local opts =
        { { text = "Trader "   .. playerColorNote(p, "■"), val = trader }
        , { text = "Merchant " .. playerColorNote(p, "●"), val = merchant }
        }

  return askText(p,q, opts, k)
end


function askWorkerTypeOrPass(p,q,workers,k)
  local opts = {}
  if workers[trader] > 0 then
    push(opts, { text = "Trader " .. playerColorNote(p, "■"), val = trader })
  end

  if workers[merchant] > 0 then
    push(opts,{ text = "Merchant " .. playerColorNote(p, "●"), val = merchant })
  end

  push(opts, { text = "Pass", val = nil })
  return askText(p, q, opts, k)
end




function askFreeSpot(g, p, q, w, r, k)
  return ask(p, ptrWorker(p,w,'∨'), q, freeSpots(g,w.shape,r), k)
end

function askFreeAdjacent(g,p,q,e,w,k)
  return ask(p, ptrWorker(p,w,'∨'), q, freeAdjacent(g,w.shape,e),k)
end

function mayPress(p,c)
  if 1 == 1 then return true end  -- XXX: testing
  if p == c then return true end
  say (playerColorBB(c) .. " cannot press " .. playerColorBB(p) ..
                                                      "'s buttons.")
  return false
end

function askOccupiedSpot(p,q,spots,k)
  return askOccupiedSpotL(p,"∧",q,spots,k)
end

-- This is different from `ask` in that it does not spawn
-- new objects for the question, but rather labels existing ones
function askOccupiedSpotL(p,lab,q,spots,k)
  local ui = {}

  local ql
  if q then ql = question(q) end


  local finished = false

  local function cleanUp()
    for _,thing in ipairs(ui) do
      DEL_DYN(thing.fun)
      thing.obj.removeButton(0)
    end
    if ql then ql.destroy() end
  end

  for _,spot in ipairs(spots) do
    local me = #ui

    local fun = DYN_GLOB(function(obj,c,alt)
      if finished then return end
      if not mayPress(p,c) then return end
      finished = true
      cleanUp()
      k(spots[me + 1])
    end)

    local obj
    if spot.edge then obj = GUI.edge[spot.edge].stops[spot.stop]
                 else obj = GUI.node[spot.node].offices[spot.office]
    end

    obj.createButton({
      label = lab,
      tooltip = playerColorBB(spot.worker.owner) .. " "
                                .. workerName(spot.worker.shape),
      font_size = 800,
      width = 700,
      height = 700,
      rotation = { 0, 180, 0 },
      position = { 0, 1, 0 },
      color = playerColor(spot.worker.owner),
      font_color = spot.worker.owner == p and playerFontColor(p) or
                                                          playerColor(p),
      click_function = fun
    })
    push(ui, { fun = fun, obj = obj })
  end

  return cleanUp
end


function askOccupiedSpotOrPass(p,q,spots,k)
  local cancelPass = nop
  local cancelPick = askOccupiedSpot(p,q,spots,function(i)
    cancelPass()
    k(i)
  end)

  cancelPass = askText(p,nil, { { text = "Pass", val = nil } },function()
    cancelPick()
    k(nil)
  end)

end



function askText(p, q, labs, k)
  local o
  local funs = {}

  local function cleanUp()
    for _,f in ipairs(funs) do
      DEL_DYN(f)
    end
    o.destroy()
  end

  o = spawnMenu(5,13,function(o)
        if q then spawnMenuItem(p,o,-1,q,nil) end

        local ix = 1
        for i,l in ipairs(labs) do
          if l.separator and i == #labs then break end -- skip end separator

          local finished = false
          local fun
          if l.separator then
            fun = nil
            ix = ix + 1
          else fun = DYN_GLOB(function (obj,c,alt)
                                    if finished then return end
                                    if not mayPress(p,c) then return end
                                    finished = true
                                    cleanUp()
                                    k(l.val)
                                  end)
          end
          if fun then push(funs,fun) end
          spawnMenuItem(p,o,ix,l.text,fun)
          ix = ix + 1
        end
      end)
  return cleanUp

end

function question(q)
  return spawnMenu(5,13,|m|spawnMenuItem(nil,m,-1,q,nil))
end


function askEdge(g, p, q, es, k)
  local opts = {}
  for _,e in ipairs(es) do
    local ed = g.map.edges[e]
    push(opts, { x = ed.x, y = ed.y, r = ed.rotation, val = ed })
  end
  ask(p, ptrTri(), q, opts, k)
end


function ask(p, mark, q, locs, k)     -- locs: {x,y,r,val}

  local ql
  if q then
    ql = question(q)
  end


  local markers = {}

  local finished = false

  local function cleanUp()
    for i,m in ipairs(markers) do
      DEL_DYN(m.fun)
      m.obj.destroy()
    end
    if ql then ql.destroy() end
  end

  local function btn(i,m)
    local fun = DYN_GLOB(function (obj,c,alt)
                  if finished then return end
                  if not mayPress(p,c) then return end
                  finished = true
                  cleanUp()
                  k(locs[i].val)
               end)
    m.createButton({
      label = mark.label,
      font_size = mark.font_size,
      width = mark.font_size,
      height = mark.font_size,
      rotation = mark.rotation,
      position = mark.position,
      color = mark.color or playerColor(p),
      font_color = mark.font_color or playerFontColor(p),
      click_function = fun
    })
    markers[i].fun = fun
  end

  for ix,i in ipairs(locs) do
    local m = {}
    m.obj = mark.spawn(p,i,|o|btn(ix,o))
    markers[ix] = m
  end

  return cleanUp

end


--------------------------------------------------------------------------------
-- Pointers

function ptrWorkerType () return
  { label = "?"
  , font_size = 600
  , rotation = { 0, 180, 0 }
  , position = { 0, 0.5, 0 }
  , spawn = function(p,loc,k)
              if loc.val then
                return spawnWorker( { owner = p, shape = loc.val }
                                  , { loc.x, 2, loc.y }
                                  , k --[[function(o)
                                      o.setScale({1,1,1})
                                      k(o)
                                    end --]]
                                  )
              else
                return spawnPass({loc.x, 2, loc.y},k)
              end
    end
  }
end



function ptrWorker(p,w,l) return
  { label = l or "?"
  , font_size = w.shape == merchant and 600 or 400
  , rotation = { 0, 180, 0 }
  , position = { 0, 0.5, 0 }
  , color = w.owner
  , font_color = (p == w.owner) and playerFontColor(p) or playerColor(p)
  , spawn = function(p,loc,k)
              return spawnWorker( w
                                , { loc.x, 2, loc.y }
                                , k
                                )
    end
  }
end


function ptrTri()
  return
    { font_size = 300
    , rotation = { 90, 90, 0 }
    , position = { 0, 0.05, -1 }
    , spawn = function(p,loc,k) return spawnObject({
                type = "BlockTriangle",
                position = { loc.x,2,loc.y },
                scale = { 1, 1, 0.2},
                rotation = { 90, loc.r, 0 },
                callback_function = function(o)
                  o.setLock(true)
                  o.setColorTint(playerColor(p))
                  o.highlightOn({0,0,0}, 5 * 60)
                  k(o)
                end
              })
              end
  , label = '>'
  }
end

function say(x)
  printToAll(x, { r = 1, g = 1, b = 1 })
end

