
-- Just place a canal on the map, does not update counters
function doPlaceCanal(g,loc,k)
  locMapInsert(g.map,loc,terCanal())
  spawn1x1(gridToWorld(loc,piece_z),function(o)
    local ui = locMapLookup(GUI.map,loc)
    ui.terrain = o
    k()
  end)
end

function doBuildCanal1x1(g)
  g.canal1 = g.canal1 - 1
  counterChange(GUI.canal1,g.canal1)
end

function doBuildCanal2x1(g)
  g.canal2 = g.canal2 - 1
  counterChange(GUI.canal2,g.canal2)
end


