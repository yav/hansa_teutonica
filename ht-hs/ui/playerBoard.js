function drawPlayer(opts) {

  let height = opts.height
  let color = opts.color

  let dom = document.createElement('div')
  dom.classList.add('player')
  dom.style.height = height + 'px'

  let img = document.createElement('img')
  img.setAttribute('src', 'img/player/' + color + '.png')
  img.classList.add('player-board')
  dom.appendChild(img)

  let wsize = height * 0.07
  { // name
    const lab = document.createElement('div')
    lab.classList.add('player-label')
    lab.classList.add(opts.color)
    lab.textContent = opts.name
    lab.style.fontSize = 0.08 * height
    dom.appendChild(lab)
  }

  { // Stats
    let layout =
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
      let info = layout[stat]
      let n    = info.x.length
      let y    = height * info.y
      for (let i = opts[stat]; i < n; ++i) {
        const worker = { shape: info.shape == 'disc' ? 'disc' : 'cube'
                       , owner: color
                       }
        const loc = { x: info.x[i]*height, y: y }
        let b = drawWorkerAt(loc,wsize,worker)
        if (info.shape == 'rombus') b.style.transform = 'rotate(45deg)'
        if (i == opts[stat])
          b.setAttribute("id",opts.color + "-" + stat)
        dom.appendChild(b)
      }
    }
  }

  // spent bonus
  if (opts.spentBonuses > 0) {
    let it = document.createElement('div')
    it.classList.add('bonus-spent')
    it.setAttribute('title', 'Spent bonus tokens')
    let lab = document.createElement('span')
    lab.textContent = opts.spentBonuses
    it.appendChild(lab)
    let style = it.style
    let dim = 0.2 * height
    style.width = dim
    style.height = dim
    style.fontSize = 0.5 * dim
    style.left = 1.20 * height
    style.top = 0.12 * height
    dom.appendChild(it)
  }


  let bar = document.createElement('div')
  bar.setAttribute('id', color + '-bar')
  bar.classList.add('player-bar')
  let barStyle = bar.style
  barStyle.left = height * 0.3;
  barStyle.bottom = height * 0.025;
  let barHeight = height * 0.18
  barStyle.height = barHeight
  dom.appendChild(bar)

  function pBox(label) {
    let b = document.createElement('div')
    b.classList.add('player-box')
    bar.appendChild(b)

    if (label) {
      let lab = document.createElement('div')
      lab.classList.add('bonus-multiplier')
      lab.textContent = label
      let sc = 0.27 * barHeight
      lab.style.fontSize = sc
      b.appendChild(lab)
    }

    return b
  }

  function drawSupplyIn(b,which,shape,name) {
    let it = drawWorker(wsize,{owner:color, shape: shape})
    it.setAttribute('id', color + '-' + which + '-' + shape)
    it.setAttribute('title',name)
    it.textContent = opts[which][shape]
    b.appendChild(it)
  }

  function drawSupply(which,name) {
    let b = pBox()
    drawSupplyIn(b,which,'cube',name)
    let sep = document.createElement('div')
    sep.classList.add('player-sep')
    sep.style.width = wsize / 2
    b.appendChild(sep)
    drawSupplyIn(b,which,'disc',name)
  }

  drawSupply('available','Available')

  { // Bonuses
    let size = barHeight * 0.9
    for (bonus in opts.bonuses) {
      let n = opts.bonuses[bonus]
      let b = pBox(n > 1 ? ('x' + n) : null)
      let it = drawBonusToken(size,bonus)
      it.setAttribute('id',color + '-bonus-' + bonus)
      b.appendChild(it)
      bar.appendChild(b)
    }
  }

  { // VP
    let it = pBox()
    it.textContent  = opts.vp + ' VP'
    it.style.fontSize = 0.4 * barHeight
    bar.appendChild(it)
  }

  drawSupply('unavailable','Uvailable')

  return dom
}


