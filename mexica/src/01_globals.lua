-- Save games and undo
local versonMaj  = 3
local versonMin  = 2
local saved_game = nil
local undo_stack = {}
local undoing    = false
local start_menu = nil -- are we in the start menu


-- Currently spawned objects
local GUI

-- Location of the board
local board_x = -5
local board_y = 2

local piece_z             = 1.2 -- flat pieces on the board
local piece_bridge_base_z = 1.5 -- for a bridge on top of a water tile
local piece_bridge_z      = 1.8 -- for a leader on a bridge
local piece_palace_z      = 1.3 -- for a leader on the palace

local menu_x  = 23
local menu_y  = 17

local undo_x  = 23
local undo_y  = -9

local start_menu_x = -1
local start_menu_y =  5


--------------------------------------------------------------------------------

-- Game phases
local setup     = 1
local age1      = 2
local age2      = 3
local finished  = 4

local phaseNames = { "Setup", "Early Period", "Late Period", "Game Over" }

-- Terrain types
local land  = 1
local canal = 2

-- Directions
local west  = 1
local north = 2
local east  = 3
local south = 4

local dirDX = { -1, 0, 1, 0 }
local dirDY = { 0, -1, 0, 1 }

local allDirs = { west, north, east, south }

-- Bridge directions
local north_south = 1
local east_west   = 2

local bridge    = 1
local temple    = 2
local district  = 3
local palace    = 4




