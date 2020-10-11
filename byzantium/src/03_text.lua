
local act_bulgar_army_text = table.concat(
    { "1. Increase Bulgar army strength by 2"
    , "2. Either:"
    , "     - Attack neighbouring city, or"
    , "     - Increase strength by 2 more."
    , ""
    , "* Strength may not exceed 11."
    , "* May only use roads."
    },"\n")

local act_fortify_text = table.concat(
    { "Add one of your fortifications"
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

local act_byz_improve_text = table.concat(
    { "Improve City (Byzantium)"
    , ""
    , "Add +1 strength to a Byzantine city."
    , ""
    , "* No need to control the city."
    , ""
    , "* City strength may not exceed 3"
    , "  (fortifications do not count)."
    }, "\n")

local act_emperor_text = table.concat(
    {
    }, "\n")

local act_byz_civil_war_text = table.concat(
    {
    }, "\n")

local act_byz_fleet_text = table.concat(
    {
    }, "\n")

local act_arab_improve_text = table.concat(
    { "Add +1 strength to an Arab city."
    , ""
    , "* No need to control the city."
    , ""
    , "* City strength may not exceed 3"
    , "  (fortifications do not count)."
    }, "\n")

local act_caliph_text = table.concat(
    { "Caliph (Arab)"
    }, "\n")

local arab_civil_war_text = table.concat(
    { "Civil War (Arab)"
    }, "\n")

local arab_fleet_text = table.concat(
    { "Fleet (Arab)"
    }, "\n")

local act_taxes_text =
[[
Spend any number of available
workers for $2/worker.

* May only be used once a round.
* Earnings may be split between treasuries.
]]


local act_bulgar_army_name    = "Bulgar Army (Byzantium/Arabs)"
local act_fortify_name        = "Fortify City (Byzantium/Arabs)"
local act_byz_improve_name    = "Improve City (Byzantium)"
local act_emperor_name        = "Emperor (Byzantium)"
local act_byz_civil_war_name  = "Civil War (Byzantium)"
local act_byz_fleet_name      = "Fleet (Byzantium)"
local act_arab_improve_name   = "Improve City (Arab)"
local act_caliph_name         = "Caliph (Arab)"
local act_arab_civil_war_name = "Civil War (Arab)"
local act_arab_fleet_name     = "Fleet (Arab)"
local act_taxes_name          = "Collect Taxes"

local action_name =
  { act_bulgar_army_name
  , act_bulgar_army_name
  , act_fortify_name
  , act_fortify_name
  , act_byz_improve_name
  , act_byz_improve_name
  , act_emperor_name
  , act_byz_civil_war_name
  , act_byz_fleet_name
  , act_arab_improve_name
  , act_arab_improve_name
  , act_arab_improve_name
  , act_caliph_name
  , act_arab_civil_war_name
  , act_arab_civil_war_name
  , act_arab_fleet_name
  , act_taxes_name
  }

local action_text =
  { act_bulgar_army_text
  , act_bulgar_army_text
  , act_fortify_text
  , act_fortify_text
  , act_byz_improve_text
  , act_byz_improve_text
  , act_emperor_text
  , act_byz_civil_war_text
  , act_byz_fleet_text
  , act_arab_improve_text
  , act_arab_improve_text
  , act_arab_improve_text
  , act_caliph_text
  , act_arab_civil_war_text
  , act_arab_civil_war_text
  , act_arab_fleet_text
  , act_taxes_text
  }


local faction_stat_name =
  { eliteArmy = "Elite Army"
  , mainArmy  = "Main Army"
  , levy      = "Levy"
  , movement  = "Movement"
  , treasury  = "Treasury"
  , vp        = "VP"
  }

