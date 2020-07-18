
function gridToWorld(loc)
  local x = (loc.col - 14) * 2 - 1
  local y = (loc.row - 9) * -2 + 1
  return Vector(x,1.2,y)
end

