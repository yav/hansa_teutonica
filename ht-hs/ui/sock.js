function srvConnect(url) {
  let ws = new WebSocket(url)

  ws.onopen = function(e) {
    console.log("Connected.")
    ws.send(new URL(window.location).searchParams.get("player"))
  }

  ws.onmessage = function(e) {
    const msg = JSON.parse(e.data)
    msg.fun(...msg.args)
  }

  ws.onclose = function(e) {
    console.log("Disconnected.")
  }

  ws.onerror = function(e) {
    console.log("error")
    console.log(e)
  }
}



