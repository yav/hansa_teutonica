local versonMaj  = 1
local versonMin  = 1
local saved_game = nil
local undo_state = nil
local undoing    = false
local start_menu = nil -- are we in the start menu



-- factions
local byzantium = 1
local arabs     = 2
local bulgars   = 3
local persians  = 4

local stat_cost = { eliteArmy = 3, mainArmy = 1, movement = 1, levy = 2 }

local faction_name =
  { "Byzantium"
  , "Arabs"
  , "Bulgars"
  , "Persia"
  }

local faction_currency =
  { "bezants"
  , "dinars"
  }

local faction_temple =
  { "church"
  , "mosk"
  }

local faction_poss =
  { "byzantine"
  , "arab"
  , "bulgar"
  , "persian"
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

-- action spaces
local bulgars_1         = 1
local bulgars_2         = 2
local fortify_1         = 3
local fortify_2         = 4
local byz_improve_1     = 5
local byz_improve_2     = 6
local emperor           = 7
local byz_civil_war     = 8
local byz_fleet         = 9
local arab_improve_1    = 10
local arab_improve_2    = 11
local arab_improve_3    = 12
local caliph            = 13
local arab_civil_war_1  = 14
local arab_civil_war_2  = 15
local arab_fleet        = 16
local taxes             = 17
local church            = 18
local mosk              = 19
local pass              = 20

local allActionSpaces =
  { bulgars_1
  , bulgars_2
  , fortify_1
  , fortify_2
  , byz_improve_1
  , byz_improve_2
  , emperor
  , byz_civil_war
  , byz_fleet
  , arab_improve_1
  , arab_improve_2
  , arab_improve_3
  , caliph
  , arab_civil_war_1
  , arab_civil_war_2
  , arab_fleet
  , taxes
  , church
  , mosk
  , pass
  }

-- attacker/defender
local attacker = 1
local defender = 2


local blocker_color = Color(0.2,0.2,0.2)




--------------------------------------------------------------------------------
local GUI
