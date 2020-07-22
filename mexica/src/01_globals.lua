-- Save games and undo
local versonMaj  = 2
local versonMin  = 1
local saved_game = nil
local undo_stack = {}
local undoing    = false


-- Currently spawned objects
local GUI

-- Location of the board
local board_x = -4.7
local board_y = 2

local piece_z          = 1.2 -- flat pieces on the board
local piece_temple_z   = 1.5 -- temples are fatter
local piece_bridge_z   = 1.8 -- for a leader on a bridge

local menu_x  = 23
local menu_y  = 17

local undo_x  = 23
local undo_y  = -7


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




