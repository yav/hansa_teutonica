
function drawBoard(height,name) {
  let h = 1000
  let dom = document.createElement('div')
  dom.classList.add('board')
  dom.style.height = h

  let img = document.createElement('img')
  img.classList.add('board-img')
  img.setAttribute('src','img/board/' + name + '.jpg')
  dom.appendChild(img)


  let sz = h/100

  let nodeLocs = {}

  let x = 0
  let y = 0
  let prev = null
  for (let i = 0; i < map.nodes.length; ++i) {
    let node = map.nodes[i]
    let id = "node-" + node.node + '-' + node.id

    if (node.node == prev) {
      x = x + 2 * sz
    }
    else {
      x = 750; y = y + 3 * sz
      let lab = document.createElement('div')
      lab.classList.add('bonus-multiplier')
      lab.textContent = node.name
      lab.style.height = 20
      lab.style.width = 100
      lab.style.left = x
      lab.style.top  = y
      y = y + 3 * sz
      dom.appendChild(lab)
    }
    prev = node.node

    let it = drawWorkerAt(x, y, sz, node.req, 'red')
    it.classList.remove('red')
    it.setAttribute("id",id)
    it.setAttribute("draggable","true")
    it.setAttribute('title',node.name + ' ' + (node.id + 1))
    if (node.vp > 0)
      it.textContent = node.vp

    let cols = [ "white", "orange", "pink", "black" ]
    it.style.backgroundColor = cols[node.priv - 1]
    it.addEventListener('dragstart', function(ev) {
      ev.dataTransfer.setDragImage(it,0,0)
    })

    it.addEventListener('dragend', function(ev) {
      it.style.left = ev.clientX
      it.style.top  = ev.clientY
    })

    dom.appendChild(it)
  }





  return dom
}


