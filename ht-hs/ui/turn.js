
function drawTurn(turn) {
  const ui = {}

  let done    = turn.actDone
  let limit   = turn.actLimit
  let bonuses = turn.bonuses

  const lab = function(l) {
    const it = document.createElement('span')
    it.classList.add('turn-label')
    it.textContent = l
    return it
  }

  const group = function(xs) {
    const g = document.createElement('span')
    g.classList.add('turn-group')
    for (let i = 0; i < xs.length; ++i) {
      g.appendChild(xs[i])
    }
    dom.appendChild(g)
    return g
  }

  const dom = document.createElement('div')
  dom.classList.add('turn')
  dom.style.fontSize = 1.5 * gui.board.fontSize

  { // undo btn
    const btn = document.createElement('span')
    btn.classList.add('question')
    btn.classList.add('undo')
    btn.innerHTML = '&#x2190;'
    btn.addEventListener('click',function() { gui.undo() })
    dom.appendChild(btn)
    gui.tooltip(btn,'Undo')
  }



  const player = lab(turn.player)
  player.classList.add('turn-player')
  player.classList.add(gui.colors[turn.player])
  group([ lab('Turn: '), player ])

  const elDone  = lab(done)
  const elLimit = lab(limit)
  group([ lab('Actions: '), elDone, lab('/'), elLimit ])

  const drawPicked = function(w) {
    const it = drawWorker(gui.board.workerSize,w)
    it.classList.add('hand')
    return it
  }

  const picked = []
  for (let i = 0; i < turn.pickedUp.length; ++i) {
    const w = turn.pickedUp[i]
    const it = drawPicked(w)
    picked[i] = it
  }
  const pickedGroup = group(picked)

  // XXX: the "hand": bonuses, picked up things, etc


  gui.panel.prepend(dom)


  // exported
  return {

    changeDone:
      function(n) {
        done = done + n
        elDone.textContent = done
      }

  , changeLimit:
      function(n) {
        limit = limit + n
        elLimit.textContent = limit
      }

  , askDone:
      function(json) {
        const btn = document.createElement('span')
        btn.classList.add('turn-player')
        btn.classList.add(gui.colors[turn.player])
        btn.textContent = json.choice.message
        gui.questionNew(group([btn]), json)
      }

  , remove:
      function() { dom.remove() }

  , removeWorkerFromHand:
      function() {
        const ch = pickedGroup.children[0]
        pickedGroup.removeChild(ch)
      }
  , addWorkerToHand:
      function(w) { pickedGroup.appendChild(drawPicked(w)) }

  }


}


