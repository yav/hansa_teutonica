function onLoad(state)
  local g = newGame({"Green","Purple","Yellow","Blue","Red" }
                   , britaniaMap
                   )
  local q = actQ()
  q.enQ(|| newGUI(g, q.next))
  q.enQ(|| test(g))
end



