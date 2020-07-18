-- Currently spawned objects
local GUI

-- Location of the board
local board_x = -4.7
local board_y = 2
local piece_z = 1.2   -- piece that is pretty much on the board

local menu_x  = 23
local menu_y  = 17


--------------------------------------------------------------------------------

-- Game state
local setup = 1
local playing = 2

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
local east_est    = 2

local leader    = 1
local temple    = 2
local district  = 3




