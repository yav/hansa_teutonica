--------------------------------------------------------------------------------
-- A map from locations to values

function locMapEmpty()
  return {}
end

function locMapInsert(locMap,loc,val)
  local row = locMap[loc.row]
  if not row then row = {}; locMap[loc.row] = row end
  row[loc.col] = val
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



