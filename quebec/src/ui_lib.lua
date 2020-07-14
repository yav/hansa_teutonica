function playerColor(p)
  if not p then return Color(0.5,0.5,0.5) end
  if p == "White" then return Color(0.9, 0.9, 0.9) end -- White is broken
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
  return playerColorNote(p,p or "")
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
  local bg  = f and {0,0,0} or {0.1,0.1,0.1}
  local fg  = {1,1,1}
  local msg = lab
  if p and f then
    msg = playerColorNote(p, "> ") .. lab .. playerColorNote(p, " <")
  end
  local hover = f and {0.3,0.3,0.3} or bg
  if p and f then
    local h = playerColor(p)
    hover = Color(h.r * 0.5, h.g * 0.5, h.b * 0.5)
  end
  menu.createButton(
    { font_size      = 300
    , font_color     = fg
    , hover_color    = hover
    , press_color    = f and {0.5,0.5,0.5} or bg
    , color          = bg
    , label          = msg
    , click_function = f or "nop"
    , position       = { 0, 1.2, -ix }
    , rotation       = { 0, 180, 0 }
    , width          = 4000
    , height         = 400
    }
  )
end

function spawnLabel(x,y,l,k)
  return spawnMenu(x,y,function(menu)
    spawnMenuItem(p,menu,0,l,nil)
    k(menu)
  end)
end



--------------------------------------------------------------------------------
-- For generating dynamic button functions

-- For "buttons" that are labels
function nop() end

-- Not a constant, but we want it to go at the top
local GLOB_FUN_COUNTER = 0

function DYN_GLOB(f)
  local nm = "SpecialPrefixDyn_" .. GLOB_FUN_COUNTER
  _G[nm] = f
  GLOB_FUN_COUNTER = GLOB_FUN_COUNTER + 1
  return nm
end

function DEL_DYN(nm)
  _G[nm] = nil
end


