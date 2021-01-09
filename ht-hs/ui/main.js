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

  const makeQuestion = function(el,val) {
    const tip = document.createElement('div')
    newQuestionExtra(tip)
    tip.classList.add('tooltip')
    tip.textContent = val.help
    tip.style.left = el.offsetLeft + 20
    tip.style.top  = el.offsetTop + 20

    // place them in the same parent so that z-indexes work correctly
    el.parentNode.appendChild(tip)
    el.classList.add('question')

    const funClick = function(ev) {
      removeQuestions()
      console.log('sending:')
      console.log(val.choice)
      ws.send(JSON.stringify(val.choice))
    }
    const funEnter = function(ev) { tip.style.display = 'inline-block' }
    const funLeave = function(ev) { tip.style.display = 'none' }
    el.addEventListener('click',funClick)
    el.addEventListener('mouseenter',funEnter)
    el.addEventListener('mouseleave',funLeave)
    return function() {
      el.classList.remove('question')
      el.removeEventListener('click',funClick)
      el.removeEventListener('mouseenter',funEnter)
      el.removeEventListener('mouseleave',funLeave)
    }
  }

  ui.questionAnnot = function(el,val) {
    questionsElems[questionsElems.length] = makeQuestion(el,val)
  }

  ui.questionNew = function(el,val) {
    newQuestionExtra(el)
    makeQuestion(el,val)
  }

  ui.container = container

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

  { // Players
    gui.players = {}
    for (let i = 0; i < game.turnOrder.length; ++i) {
      const pid = game.turnOrder[i]
      const s   = game.players[pid]
      s.height  = 120
      s.name    = pid
      const p = drawPlayer(pid,s)
      gui[pid] = p
    }
    gui.playerUI = function(x) { return gui[x ? x : playerId] }
  }

  { // Current turn
    // XXX: check for finished
    gui.turn = drawTurn(game.status)
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
      case 'done':
        gui.turn.askDone(q)
        break
    }
  }
}



