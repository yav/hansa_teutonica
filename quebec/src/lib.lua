


function setLabel(x,l)
  for _,i in ipairs(x.getButtons()) do
    i.label = l
    x.editButton(i)
  end
  effect(x)
end

function newSem()
  local todo = 0
  return {
    done = || todo == 0,
    up = function() todo = todo + 1 end,
    down = function() todo = todo - 1 end,
    wait = function(k) Wait.condition(function() k() end, || todo == 0) end
  }
end


function actQ()

  local turn = 1
  local slot = 1
  local ans = nil
  local waiting = {}
  return {
    enQ = function(f)
      local me = slot
      slot = slot + 1
      waiting[me] = Wait.condition(f, || turn == me)
    end,
    next = function(i)
      ans = i
      waiting[turn] = me
      turn = turn + 1
    end,
    stop = function()
      for i,x in pairs(waiting) do
        Wait.stop(x)
        waiting[i] = nil
      end
    end

  }
end

function push(xs,x)
  xs[#xs + 1] = x
end


function findAreas(owned)

  local repFor = {}

  local function find(l)
    local r = repFor[l]
    if not r then return l end
    local x = find(r)
    repFor[l] = x
    return x
  end

  local function union(a,b)
    local s1 = find(a)
    local s2 = find(b)
    if s1 == s2 then return end
    repFor[s1] = s2
  end

  for loc,p in pairs(owned) do
    for _,n in ipairs(neighbours(loc)) do
      if p == owned[n] then union(loc,n) end
    end
  end

  local areas = {}
  for loc,p in pairs(owned) do
    local r = find(loc)
    local pas = areas[p]
    if not pas then pas = {}; areas[p] = pas end
    local reg = pas[r]
    if not reg then reg = {}; pas[r] = reg end
    push(reg,loc)
  end

  return areas
end


-- shuffle an array of thing
function shuffle(xs)
  local n = #xs
  local used = {}
  local a = {}
  for i = 1,n do
    local x
    repeat
      x = math.random(n)
    until not used[x]
    used[x] = true
    a[i] = xs[x]
  end
  return a
end
