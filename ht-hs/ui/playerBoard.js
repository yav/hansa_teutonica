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

  let layout =
        { book:      { x: [ 1.109, 1.309, 1.554, 1.747 ]
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
        , bag:       { x: [ 2.05, 2.26, 2.50, 2.7 ]
                     , y: 0.38
                     , shape: 'rombus'
                     }
        }

  for (const stat in layout) {
    let info = layout[stat]
    let n    = info.x.length
    let y    = height * info.y
    for (let i = opts[stat]; i < n; ++i) {
      let basicShape = info.shape == 'disc' ? 'disc' : 'cube'
      let b = drawWorker(info.x[i]*height,y,wsize,basicShape,color)
      if (info.shape == 'rombus') b.style.transform = 'rotate(45deg)'
      if (i == opts[stat])
        b.setAttribute("id",opts.color + "-" + stat)
      dom.appendChild(b)
    }
  }

  let bar = document.createElement('div')
  bar.classList.add('player-bar')
  let barStyle = bar.style
  barStyle.left = height * 0.3;
  barStyle.bottom = height * 0.025;
  let barHeight = height * 0.18
  barStyle.height = barHeight + 'px'
  dom.appendChild(bar)

  function drawStats(x,which,shape) {
    let y = (barHeight - workerSize(wsize,shape) - 4) / 2
    let it = drawWorker(x,y,wsize,shape,color)
    it.setAttribute('id', color + '-' + which + '-' + shape)
    it.textContent = opts[which][shape]
    bar.appendChild(it)
  }

  { // Workers
    let x = height * 0.1
    drawStats(x,'available','cube')
    x = x + 2*wsize
    drawStats(x,'available','disc')

    x = height * 2
    drawStats(x,'unavailable','cube')
    x = x + 2*wsize
    drawStats(x,'unavailable','disc')
  }

  { // Bonuses
    let size = barHeight * 0.9
    let y = (barHeight - size)/2
    let x = 0.4 * height
    for (bonus in opts.bonuses) {
      let it = drawBonusToken (x,y,size,bonus)
      let n = opts.bonuses[bonus]
      if (n > 1) {
        let lab = document.createElement('div')
        lab.classList.add('bonus-multiplier')
        lab.textContent = 'x' + n
        let sc = 0.3
        lab.style.height = sc * size
        lab.style.width = sc * size
        lab.style.fontSize = sc * size
        lab.style.left = x + 0.8 * size
        lab.style.top = y
        bar.appendChild(lab)
      }
      bar.appendChild(it)
      x = x + 1.2 * size
    }
  }











  return dom
}


