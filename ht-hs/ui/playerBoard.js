function drawPlayer(pid,opts) {

  const ui = {}

  const height = opts.height
  const color = gui.colors[pid]

  const dom = document.createElement('div')

  dom.classList.add('player')
  dom.style.height = height + 'px'
  dom.style.fontSize = 0.08 * height

  const img = document.createElement('img')
  img.setAttribute('src', 'img/player/' + color + '.png')
  img.classList.add('player-board')
  dom.appendChild(img)

  const wsize = height * 0.07
  { // name
    const lab = document.createElement('div')
    lab.classList.add('player-label')
    lab.classList.add(color)
    lab.textContent = opts.name
    dom.appendChild(lab)
  }

   // help box
  const setHelp = function() {
    const box = document.createElement('div')
    box.classList.add('player-help')
    box.classList.add(color)
    dom.appendChild(box)
    return function(thing,help) {
      thing.addEventListener('mouseenter',function() {
        box.textContent = help
      })
      thing.addEventListener('mouseleave',function() {
        box.textContent = ''
      })
    }
  } ()

  { // Stats
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

    const statInfo = {}
    for (const stat in layout) {
      const  info = layout[stat]
      const  n    = info.x.length
      const  y    = height * info.y
      const  val  = opts[stat]
      const  doms = {}
      for (let i = val; i < n; ++i) {
        const worker = { shape: info.shape == 'disc' ? 'disc' : 'cube'
                       , owner: pid
                       }
        const loc = { x: info.x[i]*height, y: y }
        const b = drawWorkerAt(loc,wsize,worker)
        setHelp(b, stat.charAt(0).toUpperCase() + stat.slice(1) +
                                                  ' upgrade ' + worker.shape)
        if (info.shape == 'rombus') b.style.transform = 'rotate(45deg)'
        doms[i] = b
        dom.appendChild(b)
      }
      statInfo[stat] = { doms: doms, val: val }
    }

    ui.upgrade = function(stat) {
      const info = statInfo[stat]
      const doms = info.doms
      doms[info.val].remove()
      delete doms[info.val]
      info.val = info.val + 1
    }
  }

  { // spent bonus

    let spent = opts.spentBonuses
    let lab = null

    const makeSpent = function() {
      const it = document.createElement('div')
      it.classList.add('bonus-spent')
      setHelp(it,'Spent bonus tokens')
      lab = document.createElement('span')
      lab.textContent = spent
      it.appendChild(lab)
      const style = it.style
      const dim = 0.2 * height
      style.width = dim
      style.height = dim
      style.left = 1.20 * height
      style.top = 0.12 * height
      dom.appendChild(it)
    }

    if (opts.spentBonuses > 0) makeSpent()

    ui.addSpentBonus = function () {
      if (spent == 0) makeSpent()
      spent = spent + 1
      lab.textContent = spent
    }

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
    const it = drawWorker(null,{owner:pid, shape: shape})
    setHelp(it,name + ' ' + shape + 's')
    let n = opts[which][shape]

    const lab = document.createElement('span')
    lab.textContent = n
    it.appendChild(lab)

    b.appendChild(it)
    const info = { num: n, lab: lab, dom: it }
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
    return b
  }
  const available = drawSupply('available','Available')

  ui.changeWorkers = function(which,shape,delta) {
    const info = workerInfo[which][shape]
    info.num = info.num + delta
    info.lab.textContent = info.num
  }

  const setPref = function() {
    const it  = document.createElement('div')
    const img = document.createElement('img')
    img.setAttribute('src','img/player/arrow.png')
    it.appendChild(img)
    it.classList.add('player-preference')
    const imgStyle = img.style
    const style = it.style
    setHelp(it,'Place these workers')


    ui.askPreference = function(json) {
      gui.questionAnnot(it,json)
    }

    return function(shape) {
      const info = workerInfo['available'][shape]
      imgStyle.height = '2ex'
      style.top       = '2.3ex'
      style.left      = 0
      info.dom.appendChild(it)
    }
  }()
  setPref(opts.preference)
  ui.setPreference = setPref


  ui.askWorker = function(which,json) {
    const shapes = workerInfo[which]
    const info = shapes[json.choice.shape]
    gui.questionAnnot(info.dom,json)
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
      setHelp(b,'Bonus token')
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
    setHelp(it,'Victory points')
    let vp = opts.vp
    it.textContent  = vp + ' VP'
    it.style.fontSize = 0.4 * barHeight
    bar.appendChild(it)
    ui.changeVP = function(delta) {
      vp = vp + delta
      it.textContent = vp + ' VP'
    }
  }

  drawSupply('unavailable','Unavailable')

  gui.container.appendChild(dom)
  return ui
}


