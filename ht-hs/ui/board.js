
function drawBoard(height,name) {
  let h = 1000
  let dom = document.createElement('div')
  dom.classList.add('board')
  dom.style.height = h

  let img = document.createElement('img')
  img.classList.add('board-img')
  img.setAttribute('src','img/board/' + name + '.jpg')
  dom.appendChild(img)


  let offX = 350
  let offY = h / 2
  let scX  = 30
  let scY  = -28

  for (let i = 0; i < nodes.length; ++i) {
    let node = nodes[i]
    let it = drawWorkerAt( offX + scX * node.x
                         , offY + scY * node.y
                         , height/100,'disc','red')
     it.style.cursor = 'pointer'
     it.style.userSelect = 'auto'
     it.setAttribute('draggable','true')

    it.addEventListener('drag', function(ev) {
      // it.style.left = ev.clientX
      // it.style.top  = ev.clientY
    })

    it.addEventListener('dragend', function(ev) {
      it.style.left = ev.clientX - 10
      it.style.top  = ev.clientY - 5
    })

    dom.appendChild(it)
  }




  return dom
}


