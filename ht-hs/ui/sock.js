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


    // turn
  , newTurn: function(ws,t) { gui.turn.remove(); gui.turn = drawTurn(t) }
  , changeDoneActions: function(ws,n) { gui.turn.changeDone(n) }
  , changeActionLimit: function(ws,n) { gui.turn.changeLimit(n) }
  , addWorkerToHand: function(ws,w) { gui.turn.addWorkerToHand(w) }
  , removeWokerFromHand: function(ws) { gui.turn.removeWorkerFromHand() }
  }

function sendJSON(ws,obj) {
  ws.send(JSON.stringify(obj))
}

function srvConnect() {
  const info = new URL(window.location).searchParams
  const url = info.get('url')
  const actulaURL = 'ws://' + (url ? url : 'localhost:8000')
  console.log("Connecting to: " + actulaURL)
  const ws = new WebSocket(actulaURL)

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



