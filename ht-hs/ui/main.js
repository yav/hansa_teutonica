let playerId
let gui

function main() { srvConnect() }


function newGUI(ws,container) {
  container.innerHTML = ''

  const questionsExtra = []
  const questionsElems = []
  const ui = {}

  const newQuestionExtra = function(d) {
    questionsExtra[questionsExtra.length] = d
  }

  const removeQuestions = function() {
    for (let i = 0; i < questionsExtra.length; ++i) questionsExtra[i].remove()
    for (let i = 0; i < questionsElems.length; ++i) questionsElems[i]()
  }

  const tooltip = function(el,lab) {
    const tip = document.createElement('div')
    tip.classList.add('tooltip')
    tip.textContent = lab
    tip.style.left = el.offsetLeft + 20
    tip.style.top  = el.offsetTop + 20
    el.parentNode.appendChild(tip)

    const funEnter = function(ev) { tip.style.display = 'inline-block' }
    const funLeave = function(ev) { tip.style.display = 'none' }
    el.addEventListener('mouseenter',funEnter)
    el.addEventListener('mouseleave',funLeave)

    return { dom: tip, enter: funEnter, leave: funLeave }
  }

  const makeQuestion = function(el,val) {
    const tip = tooltip(el,val.help)
    newQuestionExtra(tip.dom)

    // place them in the same parent so that z-indexes work correctly
    el.classList.add('question')

    const funClick = function(ev) {
      removeQuestions()
      console.log('sending:')
      console.log(val)
      ws.send(JSON.stringify(val))
    }
    el.addEventListener('click',funClick)
    return function() {
      el.classList.remove('question')
      el.removeEventListener('click',funClick)
      el.removeEventListener('mouseenter',tip.enter)
      el.removeEventListener('mouseleave',tip.leave)
    }
  }

  ui.tooltip = tooltip

  ui.questionAnnot = function(el,val) {
    questionsElems[questionsElems.length] = makeQuestion(el,val)
  }

  ui.questionNew = function(el,val) {
    newQuestionExtra(el)
    makeQuestion(el,val)
  }

  ui.container = container

  ui.undo = function() {
    const msg = { tag: 'undo' }
    console.log('sending:')
    console.log(msg)
    ws.send(JSON.stringify(msg))
  }

  return ui
}


function uiRedraw(ws,state) {
  gui = newGUI(ws, document.getElementById('main'))

  const game = state.game ? state.game : state.finished

  { // Colors
    gui.colors = {}
    const colorIx = [ 'red', 'green', 'yellow', 'purple', 'blue' ] // XXX
    for (let i = 0; i < game.turnOrder.length; ++i) {
      const pid = game.turnOrder[i]
      gui.colors[pid] = colorIx[i]
    }
  }


  { // Board
    const board = game.board
    board.size = 700
    gui.board = drawBoard(board)
  }

  { // Token count
    const el = document.createElement('div')
    el.classList.add('token-count')
    const loc = gui.board.tokenCountSpot
    const style = el.style
    style.left = loc.x
    style.top  = loc.y

    let amt = game.tokens
    const lab = document.createElement('div')
    const sz = gui.board.bonusSize
    lab.classList.add('label')
    lab.textContent = amt
    el.appendChild(lab)
    gui.tooltip(lab,'Remaining bonus tokens')
    gui.container.appendChild(el)

    gui.changeTokenCount = function(n) {
      amt = amt + n
      if (amt < 0) amt = 0
      lab.textContent = amt
    }
  }

  { // Players
    const height = 120
    const width  = 3 * height

    gui.players = {}
    const cont = document.createElement('div')
    gui.playerContainer = cont
    cont.classList.add('player-container')
    //cont.style.width = width
    gui.container.appendChild(cont)

    for (let i = 0; i < game.turnOrder.length; ++i) {
      const pid = game.turnOrder[i]
      const s   = game.players[pid]
      s.height  = height
      s.width   = width
      s.name    = pid
      const p = drawPlayer(pid,s)
      gui[pid] = p
    }
    gui.playerUI = function(x) { return gui[x ? x : playerId] }
  }

  // End VP
  for (const i in game.endVP) gui.board.placeWorkerOnVP(i, game.endVP[i])

  gui.panel = document.createElement('div')
  gui.panel.classList.add('panel')
  gui.container.appendChild(gui.panel)

  { // Current turn
    // XXX: check for finished
    gui.turn = drawTurn(game.status)
  }

  { // Log
    gui.log = drawLog()
    const n = game.log.length
    for (let i = n-1; i >= 0; --i) {
      gui.log.addLog(game.log[i])
    }
  }



  // questions
  uiQuestions(ws, state.questions)
}


function uiQuestions(ws,qs) {
  for (let i = 0; i < qs.length; ++i) {
    const q = qs[i]
    switch(q.choice.tag) {
      case 'prefer':
        gui.playerUI().askPreference(q)
        break
      case 'active':
        gui.playerUI().askWorker('available',q)
        break
      case 'passive':
        gui.playerUI().askWorker('unavailable',q)
        break
      case 'bonus':
        break

      case 'edge-empty':
        gui.board.askEmptyEdgeSpot(q)
        break
      case 'edge-full':
        gui.board.askFullEdgeSpot(q)
        break
      case 'edge':
        gui.board.askEdge(q)
        break

      case 'node-empty':
        gui.board.askEmptyOffice(q)
        break

      case 'node-upgrade':
        gui.board.askUpgrade(q)
        break

      case 'end-vp':
        gui.board.askWorkerOnVP(q)
        break

      case 'done':
        gui.turn.askDone(q)
        break
    }
  }
}



