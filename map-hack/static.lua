local res = ""

function node(name,guid)
  local p = getObjectFromGUID(guid).getPosition()
  res = res .. string.format('\n  node("%s",%.2f,%.2f)\n', name, p.x, p.z)
end

function office(ty,lvl)
  res = res .. string.format('  office(%s,%d)\n', ty, lvl)
end

function from(x)
  res = res .. string.format('\n  from("%s")\n', x)
end

function to(name,guid,rot)
  local p = getObjectFromGUID(guid).getPosition()
  res = res .. string.format('\n    to("%s",%.2f,%.2f,%d)\n', name, p.x, p.z, rot)
end

function road(guid)
  local p = getObjectFromGUID(guid).getPosition()
  res = res .. string.format('    road(%.2f,%.2f)\n',p.x,p.z)
end

