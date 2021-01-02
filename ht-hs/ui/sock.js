const handlers =
  { redraw: uiRedraw
  , ask: uiQuestions

  , setWorkerPreference: function (ws,w) {
      gui.playerUI(w.owner).setPreference(w.shape)
    }

  , setWorkerOnEdge: function(ws,edge,spot,worker) {
      gui.board.placeWorkerOnEdge(edge,spot,worker)
    }
  , changeAvailable: function(ws,w,n) {
      gui.playerUI(w.owner).changeWorkers('available',w.shape,n)
    }
  , changeUnavailable: function(ws,w,n) {
      gui.playerUI(w.owner).changeWorkers('unavailable',w.shape,n)
    }
/*
  , changeDoneActions: uiChangeDoneActions
  , changeActionLimit: uiChangeActionLimit
*/
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



