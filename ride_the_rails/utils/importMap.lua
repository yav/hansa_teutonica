function dumpMap()
  local out = ""
  local function addLine(newLine) out = string.format("%s\n%s",out,newLine) end
  addLine("function mapUSA()")
  addLine("  local map = newMap()")
  addLine("  local function mapLoc(r,c,t)")
  addLine("     locMapInsert(map.locations,location(r,c),newSpot(t))")
  addLine("  end")
  addLine("")

  for _,o in pairs(getAllObjects()) do
    local name = o.getName()
    local pos  = o.getPosition()
    local ty = nil
    if     name == "Plains"   then ty = "terrainPlains"
    elseif name == "City"     then ty = "terrainCity"
    elseif name == "Mountain" then ty = "terrainMountains"
    end
    if ty ~= nil then
      local r,c = worldToGrid(pos[1],pos[3])
      addLine(string.format("  mapLoc(%d, %d, %s)",r,c,ty))
    end
  end
  addLine("  return map")
  addLine("end")
  print(out)
end

function worldToGrid(wx,wy)
  local side = 1
  local w    = 2   * side * math.cos(math.pi/6)
  local h    = 1.5 * side
  local r    = math.floor(0.5 + wy / h)
  local c    = math.floor(0.5 + (wx/w) - (r/2))
  return r,c
end






