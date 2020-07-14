function onLoad()
  local str = ""
  local function add(x)
    str = string.format("%s\n%s",str,x)
  end

  local nodes = {}

  for _,o in pairs(getAllObjects()) do
    if o.getName() == "city" then
      local start = true
      for n in string.gmatch(o.getDescription(),"%g+") do
        if start then
          local p = o.getPosition()
          nodes[o.getGUID()] = n
          add(string.format('  node("%s",%.2f,%.2f)',n,p[1],p[3]))
        else
          local p = string.sub(n,1,1)
          local t = string.sub(n,2,2)
          local ty = (t == 't' and 'trader') or
                     (t == 'm' and 'merchant') or 'error'
          add(string.format('  office(%s,%d)',ty,p))
        end
        start = false
      end
      add("")
    elseif o.getName() == "full" then
      local p = o.getPosition()
      add(string.format('-- full %.2f, %.2f', p[1], p[3]))
    elseif o.getName() == "invest" then
      local p = o.getPosition()
      add(string.format('-- invest %.2f, %.2f', p[1], p[3]))
    end
  end


  local edges = {}
  for _,o in pairs(getAllObjects()) do
    if o.getName() == "Edge" then
      local parts = {}
      local i = 1
      for n in string.gmatch(o.getDescription(),"%g+") do
        parts[i] = n
        i = i + 1
      end
      local src = parts[1]
      local entry = edges[src]
      if not entry then entry = {} end
      entry[#entry + 1] = { o = o, parts = parts }
      edges[src] = entry
    end
  end

  add("-- Edges --")

  for e,tos in pairs(edges) do
    add(string.format('  from("%s")', nodes[e]))
    add('')

    for _,to in ipairs(tos) do
      for i,guid in ipairs(to.parts) do
        if i == 2 then
          local p = to.o.getPosition()
          local r = to.o.getRotation()
          add(string.format('    to("%s",%.2f,%.2f,%d)',nodes[guid]
                                      , p[1], p[3], r[2]))
        elseif i > 2 then
          local o = getObjectFromGUID(guid)
          local p = o.getPosition()
          add(string.format('    %s(%.2f,%.2f)',o.getName(),p[1],p[3]))
        end
      end
      add('')
    end
  end


  print(str)
end
