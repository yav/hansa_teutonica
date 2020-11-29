
function push(arr,el)
  local n = #arr + 1
  arr[n] = el
  return n
end

function out(fmt,...)
  io.write(string.format(fmt,...))
end

local map

function list(t) return function(xs)
  if next(xs) == nil then out("[]"); return end

  local sep = "[ "
  for _,x in pairs(xs) do
    out(sep)
    t(x)
    sep = ", "
  end
  out("]")
end
end



function bonus(t)
  if     t == bonusPrintedPlace2 then out("FixedBonus BonusPlace2")
  elseif t == bonusPrintedMove2  then out("FixedBonus BonusMove2")
  elseif t == bonusPrintedGainPrivilege then out("FixedBonus BonusGainPrivilege")
  elseif t == bonusPrintedBuildInGreen then out("FixedBonus BonusBuildInGreen")
  elseif t == bonusPrintedReuse2 then out("FixedBonus BonusReuse2")
  elseif t == nil then out("NoBonus")
  else out("BadBonus %s",t)
  end
end

function bool(x)
  if x then out("True") else out("False") end
end

function int(x)
  out("%d",x)
end

function str(x)
  out("\"%s\"",x)
end

function field(sep,x,v,f)
  out("  %s %s = ", sep, x)
  f(v)
end

function exportShape(n)
  if n == trader then out ("Require Cube") else out("Require Disc") end
end

function exportAction(n)
  if     n == upgradeAction   then out("UpdgradeStat Actions")
  elseif n == upgradeBook     then out("UpdgradeStat Movement")
  elseif n == upgradeKey      then out("UpdgradeStat Keys")
  elseif n == upgradeBag      then out("UpdgradeStat Hire")
  elseif n == upgradeBuilding then out("UpdgradeStat Privilege")
  else out("GainEndGamePoints")
  end
end

function office(o)
  out("Office")
  field("{","vp",o.vp,int)
  field(",","shape",o.shape,exportShape)
  field(",","level",o.level,int)
  out("}")
end

function region(x)
  str(map.regionNames[x])
end

function onlyProvinces(xs)
  local out = {}
  for _,x in ipairs(xs) do
    if not (x == map.defaultRegion) then push(out,x) end
  end
  return out
end

function maybeRegion(x)
  if x == map.defaultRegion then out("Nothing")
  else out("Just "); region(x) end
end


function exportNode(node)
  out("Node")
  field("{","name",node.name,str)
  field(",","provinces",onlyProvinces(node.region),list(region))
  field(",","offices",node.offices,list(office))
  field(",","gateway",node.gateway,list(region))
  field(",","actions",node.action,list(exportAction))
  out("}")
end

function exportStop(stop)
  if stop.type == stopRoad then out("AnyWorker") else out("Require Disc") end
end

function exportEdge(edge)
  out("Edge")
  field("{","province",edge.region,maybeRegion)
  field(",","from",edge.from,str)
  field(",","to",edge.to,str)
  field(",","stops",edge.stops,list(exportStop))
  field(",","startBonus",edge.startingBonus,bool)
  field(",","bonus",edge.bonus,bonus)
  out("}")
end


function board(f)
  map = newMap()
  f(map)
  out("Board")
  field("{","nodes",map.nodes,list(exportNode))
  field(",","edges",map.edges,list(exportEdge))
  out("}")
end


local allMaps =
  { originalMap
  , originalMap23
  , eastMap
  , britaniaMap23
  }

board(britaniaMap)
