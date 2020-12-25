const handlers = {
}

function sendJSON(ws,obj) {
  ws.send(JSON.stringify(obj))
}

function srvConnect(url) {
  let ws = new WebSocket(url)

  ws.onopen = function(e) {
    console.log("Connected.")
    ws.send(new URL(window.location).searchParams.get("player"))
    sendJSON(ws,{ tag: "reload" })
  }

  ws.onmessage = function(e) {
    const msg = JSON.parse(e.data)
    console.log(msg)
    handlers[msg.fun](ws,...msg.args)
  }

  ws.onclose = function(e) {
    console.log("Disconnected.")
  }

  ws.onerror = function(e) {
    console.log("error")
    console.log(e)
  }
}



