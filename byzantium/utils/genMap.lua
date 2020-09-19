function genMap()
  local str = ""
  local function add(x)
    str = string.format("%s\n%s",str,x)
  end


  add("function newMap()")
  add("  local map = { cities = {}, routes = {} }")

  for _,o in pairs(getAllObjects()) do
    local name = o.getName()
    if name ~= "board" then
    local pos = o.getPosition()

    local d = o.getDescription()
    local faction = nil
    local strength = nil

    local i = 1
    for x in d:gmatch("%w+") do
      if i == 1 then faction = x
      elseif i == 2 then strength = x
      end
      i = i + 1
    end

    local stmt = string.format("  addCity(map, \"%s\",%s,%s,%3.2f,%2.2f)"
                                , name, faction, strength, pos.x, pos.z)
    add(stmt)
    end
  end
  add("end")
  log(str)
end
