

function endTurn(g)
  local n = g.currentPlayer
  if n == #g.players then endRound(g); return end
  g.currentPlayer = n + 1
  startTurn(g)
end

function endRound(g)
  -- XXX: check for scoring/end game
  g.currentPlayer = 1
  startTurn(g)
end

function startTurn(g)
  local p = g.players[g.currentPlayer]
  local s = g.playerState[p]
  s.turnAP      = 6
  s.turnSavedAP = 0
  takeAction(g,p)
end

--------------------------------------------------------------------------------

function takeAction(g,p)
  log("Take action")
  local opts = {}

  checkCanal(g,p,opts)

  push(opts, { text = "End turn", val = ||endTurn(g) })

  local s = g.playerState[p]
  local q = string.format("%s has %d AP",playerColorBB(p),s.turnAP)
  askText(p,q,opts,|f|f())
end



function checkCanal(g,p,opts)
  local onlySingle = g.canal2 == 0
  local onlyDouble = g.canal1 == 0
  if onlyDouble and onlySingle then return end -- no more canals

  local s = g.playerState[p]
  if s.AP == 0 then return end    -- we need an action point


  local canalSpots = freeCanalSpots(g.map, onlyDouble)
  if locMapIsEmpty(canalSpots) then return end -- no place on the map

  local function buildCanal()
    local q = actQ()
    local spot1
    local spot2
    q.enQ(||askMapLoc(p,"Canal location",canalSpots,nil,function(x)
      spot1 = x
      doPlaceCanal(g,spot1,q.next)
    end))

    if not onlySingle then
      q.enQ(function()
        local spots = freeLandNeigbours(g.map,spot1,false)
        if locMapIsEmpty(spots) then q.next(); return end
        local optional = onlyDouble and nil or "Pass"
        askMapLoc(p,"Second location?",spots,optional,function(loc)
          if not loc then q.next(); return end
          spot2 = loc
          doPlaceCanal(g,spot2,q.next)
        end)
      end)
    end

    q.enQ(function()
      local extra = ""
      if spot2 then
        doBuildCanal2x1(g)
        extra = string.format("and %d,%d",spot2.col,spot2.row)
      else
        doBuildCanal1x1(g)
      end
      say(string.format("%s built a canal on %d,%d %s"
                       , playerColorBB(p), spot1.col, spot1.row, extra))
      s.turnAP = s.turnAP - 1
      takeAction(g,p)
    end)
  end


  push(opts, { text = "Build canal", val = buildCanal })
end
