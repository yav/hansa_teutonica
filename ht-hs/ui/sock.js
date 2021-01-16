const handlers =
  { redraw: uiRedraw
  , ask: uiQuestions

    // board
  , setWorkerOnEdge: function(ws,edge,spot,worker) {
      gui.board.placeWorkerOnEdge(edge,spot,worker)
    }
  , removeWorkerFromEdge: function(ws,edge,spot) {
      gui.board.removeWorkerFromEdge(edge,spot)
    }
  , edgeRemoveBonus: function(ws, edge) {
      gui.board.removeBonus(edge)
    }
  , edgeSetBonus: function(ws, edge, bonus) {
      gui.board.placeBonus(edge,bonus)
    }
  , placeWorkerInOffice: function(ws,node,worker) {
      gui.board.placeWorkerInOffice(node,worker)
    }
  , placeWorkerInAnnex: function(ws,node,worker) {
      gui.board.placeWorkerInAnnex(node,worker)
    }
  , swapWorkers: function(ws,node,spot) {
      gui.board.swapWorkers(node,spot)
    }
  , setEndVP: function(ws,lvl,worker) {
      gui.board.placeWorkerOnVP(lvl,worker)
    }

    // player
  , setWorkerPreference: function (ws,w) {
      gui.playerUI(w.owner).setPreference(w.shape)
    }
  , changeAvailable: function(ws,w,n) {
      gui.playerUI(w.owner).changeWorkers('available',w.shape,n)
    }
  , changeUnavailable: function(ws,w,n) {
      gui.playerUI(w.owner).changeWorkers('unavailable',w.shape,n)
    }
  , useGateway: function(ws,g) {}
  , changeVP: function(ws,player,n) {
      gui.playerUI(player).changeVP(n)
    }
  , upgrade: function(ws,player,stat) { gui.playerUI(player).upgrade(stat) }
  , gainBonusToken: function(ws,player,token) {
      gui.playerUI(player).addBonus(token)
    }
  , useBonusToken: function(ws,player,bonus) {
      const ui = gui.playerUI(player)
      ui.removeBonus(bonus)
      ui.addSpentBonus()
    }
  , prepare: function(ws,player,msg) {
      if (player == playerId) gui.alert(msg)
    }


    // turn
  , newTurn: function(ws,t) { gui.turn.remove(); gui.turn = drawTurn(t) }
  , changeDoneActions: function(ws,n) { gui.turn.changeDone(n) }
  , changeActionLimit: function(ws,n) { gui.turn.changeLimit(n) }
  , addWorkerToHand: function(ws,w) { gui.turn.addWorkerToHand(w) }
  , removeWokerFromHand: function(ws) { gui.turn.removeWorkerFromHand() }
  , drawBonusToken: function(ws) {
      gui.turn
      gui.changeTokenCount(-1)
    }
  , placingBonus: function(ws,b) { gui.setPlacing(b) }

  // log
  , log: function(ws,m) { gui.log.addLog(m) }
  }

function sendJSON(ws,obj) {
  ws.send(JSON.stringify(obj))
}

function srvConnect() {
  const obj = new URL(window.location)
  const info = obj.searchParams
  const url = 'ws://' + obj.host + '/ws'
  console.log("Connecting to: " + url)
  const ws = new WebSocket(url)

  ws.onopen = function(e) {
    console.log('Connected.')
    playerId = info.get('player') // stored in global
    console.log("We are player: " + playerId)
    ws.send(playerId)
    sendJSON(ws,{ tag: 'reload' })
  }

  ws.onmessage = function(e) {
    const msg = JSON.parse(e.data)
    console.log('Received:')
    console.log(msg)
    handlers[msg.tag](ws,...msg.args)
  }

  ws.onclose = function(e) {
    console.log('Disconnected.')
  }

  ws.onerror = function(e) {
    console.log('error')
    console.log(e)
  }
}



