
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


  const player = lab(turn.player)
  player.classList.add('turn-player')
  player.classList.add(gui.colors[turn.player])
  group([ lab('turn: '), player ])

  const elDone  = lab(done)
  const elLimit = lab(limit)
  group([ lab('actions: '), elDone, lab('/'), elLimit ])

  // XXX: the "hand": bonuses, picked up things, etc

  gui.container.appendChild(dom)

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
        console.log('here')
        const btn = document.createElement('span')
        btn.classList.add('turn-player')
        btn.classList.add(gui.colors[turn.player])
        btn.textContent = json.message
        gui.questionNew(group([btn]), json.message, json)
      }

  , remove:
      function() { dom.remove() }
  }


}


