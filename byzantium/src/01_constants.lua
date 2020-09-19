-- factions
local byzantium = 1
local arabs     = 2
local bulgars   = 3
local persians  = 4

local faction_name =
  { "Byzantium"
  , "Arabs"
  , "Bulgars"
  , "Persia"
  }

local faction_fg_color =
  { Color(0.9, 0.9, 0.9)
  , Color(0,0,0)
  , Color(0,0,0)
  , Color(0,0,0)
  }

local faction_bg_color =
  { Color(0.5, 0, 0.5)
  , Color(0.9,0.9,0.9)
  , Color(1,0.45,0)
  , Color(0.5,0.7,0.5)
  }

-- terrain types for routes
local road      = 1
local sea       = 2
local desert    = 3


--------------------------------------------------------------------------------
local GUI
