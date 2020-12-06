function movable(it) {
  it.setAttribute("draggable","true")

  it.addEventListener('dragstart', function(ev) {
    ev.dataTransfer.setDragImage(it,0,0)
  })

  it.addEventListener('dragend', function(ev) {
    it.style.left = ev.pageX
    it.style.top  = ev.pageY
  })
}



function editor(height,name) {

  const info = layout[name]

  let h = height
  let dom = document.createElement('div')
  dom.classList.add('board')
  dom.style.height = h

  let img = document.createElement('img')
  img.classList.add('board-img')
  img.setAttribute('src', info.url)
  dom.appendChild(img)

//-- Save ----------------------------------------------------------------------
  let btn = document.createElement('button')
  btn.style.position = 'absolute'
  btn.style.left = 0
  btn.style.top = 0
  btn.textContent = 'save'

  let download = document.createElement('a')
  download.style.position = 'absolute'
  download.style.left = 50
  download.style.top = 0
  download.style.backgroundColor = 'white'
  download.style.display = 'none'
  download.textContent = 'download'
  dom.appendChild(download)

  btn.addEventListener('click', function(ev) {
    let nodes = {}
    for (let i = 0; i < map.nodes.length; ++i) {
      let node = map.nodes[i]
      let me = nodes[node.node]
      if (me === undefined) {
        me = {}
        nodes[node.node] = me
      }
      let el = document.getElementById('node-' + node.node + '-' + node.id)
      me[node.id] = { x: el.offsetLeft, y: el.offsetTop }
    }
    info.nodes = nodes

    let edges = {}
    for (let i = 0; i < map.edges.length; ++i) {
      let edge = map.edges[i]
      let me = edges[edge.edge]
      if (me === undefined) {
        me = {}
        edges[edge.edge] = me
        let el = document.getElementById('edge-' + edge.edge)
        me.x = el.offsetLeft
        me.y = el.offsetTop
        me.rotate = Number(el.dataset.rotate)
      }
      let el = document.getElementById('edge-' + edge.edge + '-' + edge.spot)
      me[edge.spot] = { x: el.offsetLeft, y: el.offsetTop }
    }
    info.edges = edges

    for (let i = 0; i < 4; ++i) {
      let el = document.getElementById('pts-' + i)
      info.pts[i] = { x: el.offsetLeft, y: el.offsetTop }
    }
    { let el = document.getElementById('bonus')
      info.bonus = { x: el.offsetLeft, y: el.offsetTop }
    }
    { const el1 = document.getElementById('house1')
      info.house1 = { x: el1.offsetLeft, y: el1.offsetTop }
      const el2 = document.getElementById('house2')
      info.house2 = { x: el2.offsetLeft, y: el2.offsetTop }
    }

    const blob = new Blob( [ 'var ' + name + ' = ' +
                                            JSON.stringify(info, null, 2) ]
                          , {type : 'text/javascript'});
    download.setAttribute('href',URL.createObjectURL(blob))
    download.setAttribute('download',name + '.js')
    download.click()


  })
  dom.appendChild(btn)
//------------------------------------------------------------------------------

  let sz = 13 // h/100


  let x0 = 750
  let x = 0
  let y = 0

  function newSection(txt,w) {
    y = y + 3 * sz
    if (y > height - 3 * sz) {
      x0 = x0 + (w + 50)
      y = 3 * sz
    }
    x = x0

    let lab = document.createElement('div')
    lab.classList.add('bonus-multiplier')
    lab.textContent = txt
    lab.style.height = 20
    lab.style.width = w
    lab.style.left = x
    lab.style.top  = y
    y = y + 3 * sz
    dom.appendChild(lab)
  }



  let prev = null
  for (let i = 0; i < map.nodes.length; ++i) {
    let node = map.nodes[i]
    let id = "node-" + node.node + '-' + node.id

    if (node.node == prev) {
      x = x + 2 * sz
    }
    else {
      newSection(node.name,100)
    }
    prev = node.node

    let thisX = x
    let thisY = y
    let ans = info.nodes
    if (ans) ans = ans[node.node]
    if (ans) ans = ans[node.id]
    if (ans) { thisX = ans.x; thisY = ans.y }

    let it = drawWorkerAt({x:thisX, y:thisY}, sz, {shape:node.req, owner:'red'})
    it.classList.remove('red')
    it.setAttribute("id",id)
    it.setAttribute('title',node.name + ' ' + (node.id + 1))
    if (node.vp > 0)
      it.textContent = node.vp

    let cols = [ "white", "orange", "pink", "black" ]
    it.style.backgroundColor = cols[node.priv - 1]
    movable(it)
    dom.appendChild(it)
  }


  prev = null
  for (let i = 0; i < map.edges.length; ++i) {
    let edge = map.edges[i]

    if (edge.edge == prev) {
      x = x + 2 * sz
    }
    else {
      newSection(edge.from + " - " + edge.to, 200)

      let thisX = x
      let thisY = y
      let thisR = 0
      let ans = info.edges
      if (ans) ans = ans[edge.edge]
      if (ans) {
        thisX = ans.x
        thisY = ans.y
        thisR = ans.rotate
      }

      let b = drawBonusTokenAt({x:thisX,y:thisY},3*sz,'act_3')
      b.setAttribute('id', 'edge-' + edge.edge)
      b.dataset.rotate = thisR
      b.style.transform = 'rotate(' + thisR + 'deg'
      b.setAttribute('title', edge.from + "-" + edge.to)
      b.addEventListener('click',function(ev) {
        let d = 10; if (ev.shiftKey) d = -10
        const r = Number(b.dataset.rotate) + d
        b.dataset.rotate = r
        b.style.transform = 'rotate(' + r + 'deg)'
      })
      movable(b)
      dom.appendChild(b)
      x = x + 4*sz
    }
    prev = edge.edge

    let cols = [ 'yellow', 'blue', 'red' ]

    let thisX = x
    let thisY = y
    let ans = info.edges
    if (ans) ans = ans[edge.edge]
    if (ans) ans = ans[edge.spot]
    if (ans) { thisX = ans.x; thisY = ans.y }

    let it = drawWorkerAt({x:thisX, y:thisY}, sz, {shape:edge.req, owner:cols[edge.prov + 1]})

    let id = "edge-" + edge.edge + '-' + edge.spot
    it.setAttribute("id",id)
    it.setAttribute('title',edge.from + '-' + edge.to + ' ' + (edge.spot+1))

    movable(it)

    dom.appendChild(it)
  }

  newSection("Misc.", 150)
  for(let i = 0; i < 4; ++i) {
    let thisX = x
    let thisY = y
    let ans = info.pts
    if (ans) ans = ans[i]
    if (ans) {
      thisX = ans.x
      thisY = ans.y
    }
    let it = drawWorkerAt({x:thisX,y:thisY},sz,{shape:'disc',owner:'yellow'})
    it.setAttribute('id','pts-' + i)
    let pts = [7,8,9,11]
    it.textContent = pts[i]
    movable(it)
    x = x + 2.5 * sz
    dom.appendChild(it)
  }

  { let thisX = x
    let thisY = y
    let ans = info.bonus
    if (ans) {
      thisX = ans.x
      thisY = ans.y
    }

    let it = drawBonusTokenAt({x:thisX,y:thisY},3*sz,'act_3')
    it.setAttribute('id','bonus')
    it.setAttribute('title','Location for restock')
    movable(it)
    dom.appendChild(it)
    x = x + 3.5 * sz
  }

  { let thisX = x
    let thisY = y
    let ans = info.house1
    if (ans) {
      thisX = ans.x
      thisY = ans.y
    }

    let it = drawWorkerAt({x:thisX,y:thisY},sz,{shape:'cube',owner:'green'})
    it.classList.remove('green')
    it.style.backgroundColor = 'black'
    it.setAttribute('id','house1')
    it.setAttribute('title','First sull counter')
    movable(it)
    dom.appendChild(it)
    x = x + 2 * sz


    ans = info.house2
    if (ans) {
      thisX = ans.x
      thisY = ans.y
    }
    it = drawWorkerAt({x:thisX,y:thisY},sz,{shape:'cube',owner:'green'})
    it.classList.remove('green')
    it.style.backgroundColor = 'black'
    it.setAttribute('id','house2')
    it.setAttribute('title','Last sull counter')
    movable(it)
    dom.appendChild(it)
    x = x + 2 * sz
  }





  return dom
}


