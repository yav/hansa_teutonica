--------------------------------------------------------------------------------
-- Library

function nop() end
function apply(f) f() end

function push(arr,el)
  local n = #arr + 1
  arr[n] = el
  return n
end

function pop(arr)
  local n = #arr
  if #arr == 0 then return nil end
  local x = arr[n]
  arr[n] = nil
  return x
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
    ans = ||ans,
    next = function(i)
      ans = i
      waiting[turn] = nil
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

function when(p,k) Wait.condition(k,p) end

-- Shuffle an array of things in place
function shuffle(xs)
  local n = #xs
  for i = 1,n-1 do
    local j = math.random(i,n)
    local t = xs[i]
    xs[i] = xs[j]
    xs[j] = t
  end
end


