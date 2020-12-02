
function drawBoard(opts) {
  const map = layout[opts.map]

  let size = opts.size
  if (!opts.size) {
    if (map.width < map.height) {
      size = window.innerHeight * 0.9
    } else {
      size = window.innerWidth * 0.70
    }
    if (size < 600) size = 600
  }

  const wi = map.width
  const hi = map.height
  const larger = Math.max(wi,hi)
  const sc = size / larger
  const sz = sc * map.worker
  const cosc = sc * hi / 1000 // coordinates are relative to height 1000

  const dom = document.createElement('div')
  dom.classList.add('board')
  dom.style.height = hi * sc
  dom.style.width  = wi * sc

  const img = document.createElement('img')
  img.classList.add('board-img')
  img.setAttribute('src', map.url)
  dom.appendChild(img)


  // full
  const fullLoc1 = map.house1
  const fullLoc2 = map.house2
  const fullDom = document.createElement('div')
  fullDom.classList.add('fullMarker')
  const fullStyle = fullDom.style
  fullStyle.width  = 1.5*sz
  fullStyle.height = 1.5*sz
  const fullDiff  = (cosc * fullLoc2.x - cosc * fullLoc1.x) / opts.fullMax
  fullStyle.left  = cosc * fullLoc1.x + opts.full * fullDiff
  fullStyle.top   = cosc * fullLoc1.y
  dom.appendChild(fullDom)

  // offices
  for (const i in opts.nodes) {
    const node = opts.nodes[i]
    const loc = map.nodes[i]
    const loc0 = loc[0]

    // annexes
    let x = cosc * loc0.x - 0.5 * sz
    let y = cosc * loc0.y
    for (let j = 0; j < node.annex.length; ++j) {
      const w = node.annex[j]
      x = x - 1.5 * workerSize(sz,w.shape)
      const it = drawWorkerAt(x, y,sz,w.shape,w.owner)
      dom.appendChild(it)
    }

    // existing offices
    for (let j = 0; j < node.office.length; ++j) {
      const w = node.office[j]
      const here = loc[j]
      if (here == undefined) {
        x = x + 1.5 * workerSize(sz,node.office[j-1].shape)
      } else {
        x = cosc * here.x
        y = cosc * here.y
      }
      const it = drawWorkerAt(x,y,sz,w.shape,w.owner)
      dom.appendChild(it)
    }
  }

  // edges
  for (const i in opts.edges) {
    const edge = opts.edges[i]
    const loc = map.edges[i]
    const bonus = edge.bonus
    if (bonus) {
      const it = drawBonusTokenAt(cosc * loc.x, cosc * loc.y,3*sz,bonus)
      it.style.transform = 'rotate(' + loc.rotate + 'deg)'
      dom.appendChild(it)
    }
    const workers = edge.workers
    for (let j = 0; j < workers.length; ++j) {
      const w = workers[j]
      if (w !== null) {
        const here = loc[j]
        const it = drawWorkerAt(cosc * here.x, cosc * here.y,sz,w.shape,w.owner)
        dom.appendChild(it)
      }
    }
  }

  // endVP
  for (let i = 0; i < opts.endVP.length; ++i) {
    const w = opts.endVP[i]
    if (w !== null) {
      const here = map.pts[i]
      const it = drawWorkerAt(cosc * here.x, cosc * here.y,sz,w.shape,w.owner)
      dom.appendChild(it)
    }
  }

  return dom
}


