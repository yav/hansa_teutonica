function boardCoord(name,size) {
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

         , ptsSpot: function(ix) {
             const it = map.pts[ix]
             return { x: cosc * it.x, y: cosc * it.y }
           }

         , fullSpot: function(ix,lim) {
            const fullLoc1 = map.house1
            const fullLoc2 = map.house2
            const fullDiff = (fullLoc2.x - fullLoc1.x) / lim
            return { x: cosc * (fullLoc1.x + ix * fullDiff)
                   , y: cosc * fullLoc1.y
                   }
            }
         }
}


function drawBoardIn(container,opts) {
  const ui    = {}
  const board = boardCoord(opts.map, opts.size)

  const dom = function() {
    const dom = document.createElement('div')
    dom.classList.add('board')
    dom.style.height = board.height
    dom.style.width  = board.width

    const img = document.createElement('img')
    img.classList.add('board-img')
    img.setAttribute('src', board.url)
    dom.appendChild(img)

    return dom
  } ()

  const question = function() {
    const questionsEmpty = []
    const questionsFull  = []

    const removeQuestions = function() {
      for (let i = 0; i < questionsEmpty.length; ++i) questionsEmpty[i].remove()
      for (let i = 0; i < questionsFull.length; ++i) {
        const info = questionsFull[i]
        const dom = info.dom
        dom.classList.remove('question')
        dom.removeEventListener('click',info.fun)
      }
    }
    const makeQuestion = function(el,f) {
      el.classList.add('question')
      const fun = function(ev) { removeQuestions(); f(ev) }
      el.addEventListener('click',fun)
      return fun
    }



    const q = {}
    q.addFull = function(el,f) {
      questionsFull[questionsFull.length] = { dom: el
                                            , fun: makeQuestion(el,f)
                                            }
    }

    q.addEmpty = function(el,f) {
      makeQuestion(el,f)
      el.classList.add('new')
      questionsEmpty[questionsEmpty.length] = el
      dom.append(el)
    }

    return q
  }()


  { // Full marker
    const el = document.createElement('div')
    el.classList.add('fullMarker')
    const style = el.style
    style.width  = board.houseSize
    style.height = board.houseSize
    const setFull = function (spot) {
      const loc = board.fullSpot(spot,opts.fullMax)
      style.left = loc.x
      style.top  = loc.y
    }
    setFull(opts.full)
    dom.appendChild(el)

    // exported:
    ui.setFull = setFull
  }


  { // offices
    const lastAnnex = {} // locaiton of last annex, indexed by node

    const nextAnnexLoc = function(node,shape) {
      let loc = lastAnnex[node]
      if (loc === undefined) {
        loc   = board.nodeSpot(node,0)
        loc.x = loc.x - 0.5 * board.workerSize
      }
      loc.x = loc.x - 1.5 * workerSize(board.workerSize,shape)
      return loc
    }

    const addAnnex = function(node,worker) {
      const loc = nextAnnexLoc(node,worker.shape)
      const el  = drawWorkerAt(loc,board.workerSize,worker)
      lastAnnex[node] = loc
      dom.appendChild(el)
    }

    const placedOffices = {} // info about offices in a node

    const nextOfficeLoc = function(node) {
      let placed = placedOffices[node]
      if (placed === undefined) placed = []
      const spot = placed.length

      let loc = board.nodeSpot(node,spot)
      if (loc === undefined) {
        const last = placed[spot - 1]
        loc = { x: last.loc.x + 1.5 * workerSize(board.workerSize,last.shape)
              , y: last.loc.y
              }
      }
      return loc
    }

    const addOffice = function(node,worker) {
      const loc = nextOfficeLoc(node)
      const el  = drawWorkerAt(loc,board.workerSize,worker)
      let placed = placedOffices[node]
      if (placed === undefined) { placed = []; placedOffices[node] = placed }
      placed[placed.length] = { loc: loc, shape: worker.shape, dom: el }
      dom.appendChild(el)
    }

    for (const i in opts.nodes) {
      const node = opts.nodes[i]
      for (let j = 0; j < node.annex.length; ++j) addAnnex(i,node.annex[j])
      for (let j = 0; j < node.office.length; ++j) addOffice(i,node.office[j])
    }

    // exported
    ui.placeWorkerInAnnex  = addAnnex
    ui.placeWorkerInOffice = addOffice
    // XXX: swap office, questions
  }


  { // edges

    const placedWorkers = {}

    const makeWorker = function(edge,spot,worker) {
      const loc = board.edgeSpot(edge,spot)
      return drawWorkerAt(loc,board.workerSize,worker)
    }

    const placeWorker = function(edge,spot,worker) {
      const el = makeWorker(edge,spot,worker)
      dom.appendChild(el)
      let placed = placedWorkers[edge]
      if (placed === undefined) { placed = {}; placedWorkers[edge] = placed }
      placed[spot] = el
    }

    const askEmptyWorker = function(edge,spot,worker) {
      question.addEmpty(makeWorker(edge,spot,worker),function(ev) {
        console.log('empty spot',edge,spot)
      })
    }

    const askFullWorker = function(edge,spot) {
      question.addFull(placedWorkers[edge][spot],function(ev) {
        console.log('full spot',edge,spot)
      })
    }

    const placedBonuses = {}

    const placeBonus = function(edge,bonus) {
      const loc = board.bonusSpot(edge)
      const el  = drawBonusTokenAt(loc, board.bonusSize, bonus)
      el.style.transform = 'rotate(' + loc.rotate + 'deg)'
                                              // assume no other trnasforms
      dom.appendChild(el)
      placedBonuses[edge] = el
    }


    // Initialize board
    for (const i in opts.edges) {
      const edge = opts.edges[i]
      if (edge.bonus) placeBonus(i,edge.bonus)

      const workers = edge.workers
      for (let j = 0; j < workers.length; ++j) placeWorker(i,j,workers[j])
    }


    // exported:
    ui.placeWorkerOnEdge = placeWorker
    ui.askEmptyEdgeSpot  = askEmptyWorker
    ui.askFullEdgeSpot   = askFullWorker
    ui.placeBonus        = placeBonus
  }


  { // endVP
    const placeWorker = function(spot, worker) {
      const loc = board.ptsSpot(spot)
      const el = drawWorkerAt(loc, board.workerSize, worker)
      dom.appendChild(el)
    }

    const askWorker = function(spot, worker) {
      const loc = board.ptsSpot(spot)
      const el = drawWorkerAt(loc, board.workerSize, worker)
      question.addEmpty(el,function(ev) {
        console.log('vp spot',spot)
      })
    }

    for (const i in opts.endVP) placeWorker(i,opts.endVP[i])

    // exported:
    ui.placeWorkerOnVP = placeWorker
    ui.askWorkerOnVP = askWorker
  }


  container.appendChild(dom)
  return ui
}


