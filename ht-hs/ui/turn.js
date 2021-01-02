
function drawTurnIn(container,turn) {
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
    return g
  }

  const dom = document.createElement('div')
  dom.classList.add('turn')


  const player = lab(turn.player)
  player.classList.add('turn-player')
  player.classList.add(gui.colors[turn.player])
  dom.appendChild(group([ lab('turn: '), player ]))

  const elDone  = lab(done)
  const elLimit = lab(limit)
  dom.appendChild(group([ lab('actions: '), elDone, lab('/'), elLimit ]))

  // XXX: the "hand": bonuses, picked up things, etc

  container.appendChild(dom)

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
  }


}


