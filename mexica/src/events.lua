function onLoad()
  local q = actQ()
  local ps = { "Orange", "Brown", "White" }
  local g = newGame(ps)

  q.enQ(||newGUI(g, q.next))
  q.enQ(||test(g))
end

function test(g)
  startTurn(g)
end
