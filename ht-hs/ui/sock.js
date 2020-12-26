const handlers = {
  redraw: uiRedraw
}

function sendJSON(ws,obj) {
  ws.send(JSON.stringify(obj))
}

function srvConnect() {
  const info = new URL(window.location).searchParams
  const url = info.get('url')
  console.log(url)
  const ws = new WebSocket('ws://' + (url ? url : 'localhost:8000'))

  ws.onopen = function(e) {
    console.log('Connected.')
    ws.send(info.get('player'))
    sendJSON(ws,{ tag: 'reload' })
  }

  ws.onmessage = function(e) {
    const msg = JSON.parse(e.data)
    console.log(msg)
    handlers[msg.fun](ws,...msg.args)
  }

  ws.onclose = function(e) {
    console.log('Disconnected.')
  }

  ws.onerror = function(e) {
    console.log('error')
    console.log(e)
  }
}



