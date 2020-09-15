

-- Choose one of the given cities
function askCity(cityNames,k)
end

-- Ask text choices, if there is only one option, just select it
function askTextQuick(opts,k)
  if #opts == 1 then
    k(opts[1].val)
    return
  end

  -- XXX: ask question
end
