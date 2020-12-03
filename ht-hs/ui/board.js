function boardInfo(name,size) {
  const map = layout[name]
  const wi = map.width
  const hi = map.height
  const larger = Math.max(wi,hi)
  const sc = size / larger
  const sz = sc * map.worker
  const cosc = sc * hi / 1000 // coordinates are relative to height 1000

  return { width:      wi * sc
         , height:     hi * sc
         , url:        map.url
         , workerSize: sz
         , bonusSize:  3 * sz
         , houseSize:  1.5 * sz
         , coordScale: cosc
         , layout: map
         , nodeSpot: function(node,ix) {
             const it = map.nodes[node][ix]
             if (it === undefined) return undefined
             return { x: cosc * it.x, y: cosc * it.y }
           }
         , edgeSpot: function(edge,ix) {
             const it = map.edges[edge][ix]
             return { x: cosc * it.x, y: cosc * it.y }
           }
         , bonusSpot: function(edge) {
             const it = map.edges[edge]
             return { x: cosc * it.x, y: cosc * it.y, rotate: it.rotate }
           }
         , ptsSpot: function(i) {
             const it = map.pts[i]
             return { x: cosc * it.x, y: cosc * it.y }
           }
         }
}


function drawBoard(opts) {
  const info = boardInfo(opts.map, opts.size)

  const dom = document.createElement('div')
  dom.classList.add('board')
  dom.style.height = info.height
  dom.style.width  = info.width

  const img = document.createElement('img')
  img.classList.add('board-img')
  img.setAttribute('src', info.url)
  dom.appendChild(img)


  // full
  const fullLoc1 = info.layout.house1
  const fullLoc2 = info.layout.house2
  const fullDom = document.createElement('div')
  fullDom.classList.add('fullMarker')
  const fullStyle = fullDom.style
  fullStyle.width  = info.houseSize
  fullStyle.height = info.houseSize
  const fullDiff  = info.coordScale * (fullLoc2.x - fullLoc1.x) / opts.fullMax
  fullStyle.left  = info.coordScale * fullLoc1.x + opts.full * fullDiff
  fullStyle.top   = info.coordScale * fullLoc1.y
  dom.appendChild(fullDom)

  // offices
  for (const i in opts.nodes) {
    const node = opts.nodes[i]
    // const loc = info.layout.nodes[i]
    const loc0 = info.nodeSpot(i,0)

    // annexes
    let x = loc0.x - 0.5 * info.workerSize
    let y = loc0.y
    for (let j = 0; j < node.annex.length; ++j) {
      const w = node.annex[j]
      x = x - 1.5 * workerSize(info.workerSize,w.shape)
      const it = drawWorkerAt(x,y,info.workerSize,w.shape,w.owner)
      dom.appendChild(it)
    }

    // existing offices
    for (let j = 0; j < node.office.length; ++j) {
      const w = node.office[j]
      const here = info.nodeSpot(i,j)
      if (here === undefined) {
        x = x + 1.5 * workerSize(info.workerSize,node.office[j-1].shape)
      } else {
        x = here.x
        y = here.y
      }
      const it = drawWorkerAt(x,y,info.workerSize,w.shape,w.owner)
      dom.appendChild(it)
    }
  }

  // edges
  for (const i in opts.edges) {
    const edge = opts.edges[i]
    const bonus = edge.bonus
    if (bonus) {
      const spot = info.bonusSpot(i)
      const it = drawBonusTokenAt(spot.x, spot.y, info.bonusSize,bonus)
      it.style.transform = 'rotate(' + spot.rotate + 'deg)'
      dom.appendChild(it)
    }
    const workers = edge.workers
    for (let j = 0; j < workers.length; ++j) {
      const w = workers[j]
      if (w !== null) {
        const here = info.edgeSpot(i,j)
        const it = drawWorkerAt(here.x, here.y,info.workerSize,w.shape,w.owner)
        it.setAttribute('id','edge-' + i + '-' + j)
        dom.appendChild(it)
      }
    }
  }

  // endVP
  for (let i = 0; i < opts.endVP.length; ++i) {
    const w = opts.endVP[i]
    if (w !== null) {
      const here = info.ptsSpot(i)
      const it = drawWorkerAt(here.x, here.y, info.workerSize, w.shape,w.owner)
      dom.appendChild(it)
    }
  }

  return dom
}


