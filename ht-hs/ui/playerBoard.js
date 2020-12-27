function drawPlayerIn(container,opts) {

  const ui = {}

  const height = opts.height
  const color = opts.color

  const dom = document.createElement('div')

  dom.classList.add('player')
  dom.style.height = height + 'px'

  const img = document.createElement('img')
  img.setAttribute('src', 'img/player/' + color + '.png')
  img.classList.add('player-board')
  dom.appendChild(img)

  const wsize = height * 0.07
  { // name
    const lab = document.createElement('div')
    lab.classList.add('player-label')
    lab.classList.add(opts.color)
    lab.textContent = opts.name
    lab.style.fontSize = 0.08 * height
    dom.appendChild(lab)
  }

  { // Stats
  // XXX: change methods
    const layout =
          { movement:  { x: [ 1.109, 1.309, 1.554, 1.747 ]
                       , y: 0.427
                       , shape: 'disc'
                       }
          , privilege: { x: [ 0.273, 0.440, 0.608, 0.775 ]
                       , y: 0.533
                       , shape: 'cube'
                       }
          , keys:      { x: [ 0.255, 0.440, 0.613, 0.805, 1.002 ]
                       , y: 0.10
                       , shape: 'rombus'
                       }
          , actions:   { x: [ 1.555, 1.760, 1.963, 2.18, 2.38, 2.6 ]
                       , y: 0.15
                       , shape: 'rombus'
                       }
          , hire:      { x: [ 2.05, 2.26, 2.50, 2.7 ]
                       , y: 0.38
                       , shape: 'rombus'
                       }
          }

    for (const stat in layout) {
      const  info = layout[stat]
      const  n    = info.x.length
      const  y    = height * info.y
      for (let i = opts[stat]; i < n; ++i) {
        const worker = { shape: info.shape == 'disc' ? 'disc' : 'cube'
                       , owner: color
                       }
        const loc = { x: info.x[i]*height, y: y }
        const b = drawWorkerAt(loc,wsize,worker)
        if (info.shape == 'rombus') b.style.transform = 'rotate(45deg)'
        if (i == opts[stat])
          b.setAttribute("id",opts.color + "-" + stat)
        dom.appendChild(b)
      }
    }
  }

  // spent bonus
  // XXX: change methods
  if (opts.spentBonuses > 0) {
    const it = document.createElement('div')
    it.classList.add('bonus-spent')
    it.setAttribute('title', 'Spent bonus tokens')
    const lab = document.createElement('span')
    lab.textContent = opts.spentBonuses
    it.appendChild(lab)
    const style = it.style
    const dim = 0.2 * height
    style.width = dim
    style.height = dim
    style.fontSize = 0.5 * dim
    style.left = 1.20 * height
    style.top = 0.12 * height
    dom.appendChild(it)
  }


  // Player info bar
  const bar = document.createElement('div')
  bar.classList.add('player-bar')
  const barStyle = bar.style
  barStyle.left = height * 0.3;
  barStyle.bottom = height * 0.025;
  const barHeight = height * 0.18
  barStyle.height = barHeight
  dom.appendChild(bar)

  function pBox() {
    const b = document.createElement('div')
    b.classList.add('player-box')
    bar.appendChild(b)
    return b
  }

  const workerInfo = { available: {}, unavailable: {} }

  function drawSupplyIn(b,which,shape,name) {
    const it = drawWorker(wsize,{owner:color, shape: shape})
    it.setAttribute('title',name)
    let n = opts[which][shape]
    it.textContent = n
    if (opts.preference == shape) it.classList.add('player-preference')
    b.appendChild(it)
    const info = { num: n, dom: it }
    workerInfo[which][shape] = info
  }

  function drawSupply(which,name) {
    const b = pBox()
    drawSupplyIn(b,which,'cube',name)
    const sep = document.createElement('div')
    sep.classList.add('player-sep')
    sep.style.width = wsize / 2
    b.appendChild(sep)
    drawSupplyIn(b,which,'disc',name)
  }
  drawSupply('available','Available')

  ui.changeWorkers = function(which,shape,delta) {
    const info = workerInfo[which][shape]
    info.num = info.num + delta
    info.dom.textContent = info.num
  }

  ui.setPreference = function(shape) {
    for (const which in workerInfo) {
      const shapes = workerInfo[which]
      for (sh in shapes) {
        const info = shapes[sh]
        if (sh == shape) {
          info.dom.classList.add('player-preference')
        } else {
          info.dom.classList.remove('player-preference')
        }
      }
    }
  }


  { // Bonuses
    const bonusInfo = {}
    const size = barHeight * 0.9

    function newBonus(bonus,n) {
      const info = {}
      bonusInfo[bonus] = info

      const b = pBox()
      const lab = document.createElement('div')
      lab.classList.add('bonus-multiplier')
      lab.textContent = n > 1 ? ('x' + n) : ''
      const sc = 0.3 * barHeight
      lab.style.fontSize = sc
      b.appendChild(lab)

      const it = drawBonusToken(size,bonus)
      b.appendChild(it)

      info.dom = b
      info.num = n
      info.lab = lab
      bar.appendChild(b)
    }

    for (bonus in opts.bonuses) {
      newBonus(bonus,opts.bonuses[bonus])
    }

    ui.addBonus = function(bonus) {
      const info = bonusInfo[bonus]
      if (!info) {
        newBonus(bonus,1)
      } else {
        info.num = info.num + 1
        if (info.num > 1) {
          info.lab.textContent = 'x' + info.num
        }
      }
    }

    ui.removeBonus = function(bonus) {
      const info = bonusInfo[bonus]
      if (!info) return
      info.num = info.num - 1
      if (info.num <= 0) {
        info.dom.remove()
        delete bonusInfo[bonus]
      } else {
        info.lab.textContent = info.num > 1? ('x' + info.num) : ''
      }
    }
  }

  { // VP
    const it = pBox()
    let vp = opts.vp
    it.textContent  = vp + ' VP'
    it.style.fontSize = 0.4 * barHeight
    bar.appendChild(it)
    ui.changeVP = function(delta) {
      vp = vp + delta
      it.textContent = vp + ' VP'
    }
  }

  drawSupply('unavailable','Uvailable')

  { // current player
    let lab = null

    // XXX: change info about the current turn
    ui.setCurrrent = function(turn) {
      if (turn) {
        dom.classList.add('player-active')
        lab = document.createElement('div')
        lab.classList.add('player-active-box')
        lab.textContent = turn.actDone + '/' + turn.actLimit
        dom.appendChild(lab)
      } else {
        dom.classList.remove('player-active')
        if (lab) lab.remove()
      }
    }
  }



  container.appendChild(dom)
  return ui
}


