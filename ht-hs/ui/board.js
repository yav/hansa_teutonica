
function drawBoard(opts) {
  const dom = document.createElement('div')
  dom.classList.add('board')
  dom.style.height = opts.height

  const img = document.createElement('img')
  img.classList.add('board-img')
  img.setAttribute('src','img/board/' + opts.map+ '.jpg')
  dom.appendChild(img)

  const sz = 10

  // full
  const fullLoc1 = layout.house1
  const fullLoc2 = layout.house2
  const fullDom = document.createElement('div')
  fullDom.classList.add('fullMarker')
  const fullStyle = fullDom.style
  fullStyle.width  = 1.5*sz
  fullStyle.height = 1.5*sz
  const fullDiff = (fullLoc2.x - fullLoc1.x) / opts.fullMax
  fullStyle.left  = fullLoc1.x + opts.full * fullDiff
  fullStyle.top   = fullLoc1.y
  dom.appendChild(fullDom)

  // offices
  for (const i in opts.nodes) {
    const node = opts.nodes[i]
    const loc = layout.nodes[i]
    const loc0 = loc[0]
    let x = loc0.x - 0.5 * sz
    let y = loc0.y
    for (let j = 0; j < node.annex.length; ++j) {
      const w = node.annex[j]
      x = x - 1.5 * workerSize(sz,w.shape)
      const it = drawWorkerAt(x,y,sz,w.shape,w.owner)
      dom.appendChild(it)
    }

    for (let j = 0; j < node.office.length; ++j) {
      const w = node.office[j]
      const here = loc[j]
      if (here == undefined) {
        x = x + 1.5 * workerSize(sz,node.office[j-1].shape)
      } else {
        x = here.x
        y = here.y
      }
      const it = drawWorkerAt(x,y,sz,w.shape,w.owner)
      dom.appendChild(it)
    }
  }

  // edges
  for (const i in opts.edges) {
    const edge = opts.edges[i]
    const loc = layout.edges[i]
    const bonus = edge.bonus
    if (bonus) {
      const it = drawBonusTokenAt(loc.x,loc.y,3*sz,bonus)
      it.style.transform = 'rotate(' + loc.rotate + 'deg)'
      dom.appendChild(it)
    }
    const workers = edge.workers
    for (let j = 0; j < workers.length; ++j) {
      const w = workers[j]
      if (w !== null) {
        const here = loc[j]
        const it = drawWorkerAt(here.x,here.y,sz,w.shape,w.owner)
        dom.appendChild(it)
      }
    }
  }

  // endVP
  for (let i = 0; i < opts.endVP.length; ++i) {
    const w = opts.endVP[i]
    if (w !== null) {
      const here = layout.pts[i]
      const it = drawWorkerAt(here.x,here.y,sz,w.shape,w.owner)
      dom.appendChild(it)
    }
  }

  return dom
}


