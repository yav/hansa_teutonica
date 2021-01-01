let playerId
let gui

function main() { srvConnect() }

function newGUI(ws) {
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

  const makeQuestion = function(el,q,val) {
    const tip = document.createElement('div')
    newQuestionExtra(tip)
    tip.classList.add('tooltip')
    tip.textContent = q

    el.appendChild(tip)
    el.classList.add('question')

    const funClick = function(ev) {
      removeQuestions()
      console.log('sending:')
      console.log(val)
      ws.send(JSON.stringify(val))
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

  ui.questionAnnot = function(el,q,val) {
    questionsElems[questionsElems.length] = makeQuestion(el,q,val)
  }

  ui.questionNew = function(el,q,val) {
    questionsExtra(el)
    makeQuestion(el,q,val)
  }

  return ui
}


function uiRedraw(ws,state) {
  const body = document.getElementById('main')
  body.innerHTML = ''

  gui = newGUI(ws)

  const game = state.game

  { // Board
    const board = game.board
    board.size = 700
    gui.board = drawBoardIn(body,board)
  }

  { // Players
    const colorIx = [ 'red', 'green', 'yellow', 'purple', 'blue' ] // XXX
    gui.players = {}
    for (let i = 0; i < game.turnOrder.length; ++i) {
      const pid = game.turnOrder[i]
      const s   = game.players[pid]
      s.height  = 120
      s.color   = colorIx[i]
      s.name    = pid
      const p = drawPlayerIn(body,s)
      gui[pid] = p
    }
    gui.playerUI = function(x) { return gui[x ? x : playerId] }
  }

  { // Current turn
    const status = game.status
    if (status.tag == 'active') {
      const turn = status.turn
      const cur = gui[turn.player]
      cur.setCurrrent(turn)
    }
  }

  // questions
  uiQuestions(ws, state.questions)
}

function uiQuestions(ws,qs) {
  for (let i = 0; i < qs.length; ++i) {
    const q = qs[i]
    switch(q.tag) {
      case 'prefer':
        gui.playerUI().askPreference(q.worker,q)
        break
      case 'active':
        gui.playerUI().askWorker('available',q.worker,'tool tip',q)
        break
      case 'passive':
        break
      case 'bonus':
        break
      case 'edge-empty':
        break
      case 'edge-full':
        break
      case 'done':
        break
    }
  }
}


function uiSetWorkerPreference(ws,w) {
  const ui = gui.playerUI(w.owner).setPreference(w.shape)
}
