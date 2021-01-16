function drawLog() {
  const dom = document.createElement('div')
  dom.classList.add('log')
  dom.style.fontSize = 1.5 * gui.board.fontSize
  gui.panel.appendChild(dom)

  let turnBox = null
  let actionBox = null
  let box

  const lab = function(x,cl) {
    const el = document.createElement('span')
    el.textContent = x
    if (cl !== undefined)
      for (let i = 0; i < cl.length; ++i)
        el.classList.add(cl[i])
    box.appendChild(el)
  }

  const sayEdge = function(edgeId,spot) {
    const nodes = gui.board.edgeNodes[edgeId]
    const from  = gui.board.nodeNames[nodes[0]]
    const to    = gui.board.nodeNames[nodes[1]]
    let msg = from + '-' + to
    if (spot !== undefined) {
      msg = msg + ', spot ' + spot
    }
    lab(msg,['log-unit'])
  }

  const sayNode = function(nodeId) {
    lab(gui.board.nodeNames[nodeId],['log-unit'])
  }

  const sayWorker = function(worker) {
    box.appendChild(drawWorker(gui.board.workerSize,worker))
  }

  const sayPlayer = function(player) {
    lab(player, [ 'turn-player', gui.colors[player]])
  }

  const sayBonus = function(bonus) {
    box.appendChild(drawBonusToken(gui.board.bonusSize,bonus))
  }

  const ui = {}

  ui.StartTurn = function(player) {
    turnBox = document.createElement('div')
    turnBox.classList.add('log-turn')
    dom.prepend(turnBox)
    sayPlayer(player)
    lab('\'s turn')
    return true
  }

  ui.EndTurn = function() {
    turnBox = null
    return false
  }

  ui.StartAction = function() {
    actionBox = document.createElement('div')
    actionBox.classList.add('log-action')
    const it = turnBox ? turnBox : dom
    it.appendChild(actionBox)
    return false
  }

  ui.EndAction = function() {
    actionBox = null
    return false
  }

  ui.PickUp = function(worker,edge,spot) {
    lab('Picked-up ')
    sayWorker(worker)
    lab(' from ')
    sayEdge(edge,spot)
    return true
  }

  ui.PlaceWorker = function(worker,edge,spot) {
    lab('Placed ')
    sayWorker(worker)
    lab(' on ')
    sayEdge(edge,spot)
    return true
  }

  ui.MoveWorkerTo = function(edge,spot,worker) {
    lab('Moved ')
    sayWorker(worker)
    lab(' to ')
    sayEdge(edge,spot)
    return true
  }

  ui.ReplaceWorker = function(workerOld,workerNew,edge,spot) {
    lab('Replaced ')
    sayWorker(workerOld)
    lab(' with ')
    sayWorker(workerNew)
    lab(' on ')
    sayEdge(edge,spot)
    return true
  }

  ui.CompleteRoute = function(edge) {
    lab('Completed ')
    sayEdge(edge)
    return true
  }

  ui.BuildOffice = function(node,worker) {
    sayWorker(worker)
    lab (' established office in ')
    sayNode(node)
    return true
  }

  ui.EvHire = function(worker,number) {
    if (number == 0) return
    lab('Hired ' + number + ' ')
    sayWorker(worker)
  }

  ui.Retire = function(worker,number) {
    lab('Retired ' + number + ' ')
    sayWorker(worker)
    return true
  }

  ui.GainVP = function(player,vp) {
    sayPlayer(player)
    lab(' gained ' + vp + ' VP')
    return true
  }

  ui.Upgraded = function(player,stat) {
    lab('Upgraded ' + stat)
    return true
  }

  ui.Invested = function(nodeId,points,worker) {
    lab('Placed ')
    sayWorker(worker)
    lab(' on ')
    sayNode(node)
    lab(' ' + points + ' VP')
    return true
  }

  ui.PlacedBonus = function(edge,bonus) {
    lab('Placed ')
    sayBonus(bonus)
    lab(' on ')
    sayEdge(edge)
    return true
  }

  ui.UsedBonus = function(bonus) {
    lab('Used ')
    sayBonus(bonus)
    return true
  }

  ui.SwappedWorkers = function(node,spot,bonus) {
    lab('Used ')
    sayBonus(bonus)
    lab(' on ')
    sayNode(node)
    lab(', spot ' + spot)
    return true
  }

  ui.BuildAnnnex = function(node,worker,bonus) {
    lab('Used ')
    sayBonus(bonus)
    lab(' to build ')
    sayWorker(worker)
    lab(' annex in ')
    sayNode(node)
    return true
  }


  const handler = hsEvent(ui)

  return {
    addLog: function(msg) {

      box = document.createElement('div')
      box.classList.add('log-item')

      console.log(msg)
      if (handler(msg)) {
        const el = actionBox ? actionBox
                 : (turnBox   ? turnBox : dom)
        el.appendChild(box)
      }
    }
  }
}


