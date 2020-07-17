print("function newMap()")
print("  local map = locMapEmpty()")
local row = 1
for line in io.lines("map.txt") do
  local col = 1
  for c in line:gmatch(".") do
    print(string.format("  locMapInsert(map,location(%d,%d),%s())"
                       , row, col, c == '.' and "terCanal" or "terLand"))
    col = col + 1
  end
  row = row + 1
end
print("  return map")
print("end")
