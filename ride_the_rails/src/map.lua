function newMap()
  return
    { locations = locMapEmpty
    , disconnected = locMapEmpty
      -- maps a location to the neighbours that are disconnected from it. 
      -- since walls are generally two way, each wall should appear twice
      -- in the map once for each direction.
    }
end



