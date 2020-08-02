--------------------------------------------------------------------------------
-- A map from locations to values


function location(row,col)
  return { row = row, col = col }
end

function formatLocation(l)
  return string.format("%s,%s",l.col,l.row)
end

function sameLocation(x,y)
  return x.row == y.row and x.col == y.col
end


function locMapEmpty()
  return { }
end

function locMapIsEmpty(locMap)
  for _ in locsIn(locMap) do
    return false
  end
  return true
end

function locMapInsert(locMap,loc,val)
  if val == nil then locMapDelete(locMap,loc); return end

  local row = locMap[loc.row]
  if not row then row = {}; locMap[loc.row] = row end
  row[loc.col] = val
end

function locMapDelete(locMap,loc)
  local row = locMap[loc.row]
  if row == nil then return end
  row[loc.col] = nil
  if next(row) == nil then locMap[loc.row] = nil end
end

function locMapLookup(locMap,loc)
  local row = locMap[loc.row]
  if not row then return nil end
  return row[loc.col]
end

function locMapIter(locMap,loc)
  local r
  local row
  if not loc then
    r,row = next(locMap,nil)
    if not r then return nil end
  else
    r   = loc.row
    row = locMap[r]
  end

  local c = loc and loc.col or nil

  while true do
    local val
    c,val = next(row,c)
    if c then return location(r,c),val end
    r,row = next(locMap,r)
    if not r then return nil end
    c     = nil
  end
end

function locsIn(locMap)
  return locMapIter,locMap,nil
end
--------------------------------------------------------------------------------



