function drawPlayer(height, color) {
  let dom = document.createElement('div')
  dom.classList.add('player')
  dom.style.height = height + 'px'

  let img = document.createElement('img')
  img.setAttribute('src', 'img/player/' + color + '.png')
  img.classList.add('player-board')
  dom.appendChild(img)

  let wsize = height * 0.07

  let bookX = [ 1.109, 1.309, 1.554, 1.747 ]
  let bookY = height * 0.427
  for (let i = 0; i < 4; ++i) {
    let b = drawWorker(bookX[i]*height,bookY,wsize,'disc',color)
    dom.appendChild(b)
  }

  let privY    = height * 0.533
  let privX    = [ 0.273, 0.440, 0.608, 0.775 ]
  for (let i = 0; i < 4; ++i) {
    let b = drawWorker(privX[i] * height,privY,wsize,'cube',color)
    dom.appendChild(b)
  }

  let keyY    = height * 0.10
  let keyX    = [ 0.255, 0.440, 0.613, 0.805, 1.002 ]
  for (let i = 0; i < 5; ++i) {
    let b = drawWorker(keyX[i] * height,keyY,wsize,'cube',color)
    b.style.transform = 'rotate(45deg)'
    dom.appendChild(b)
  }

  let actY    = height * 0.15
  let actX    = [ 1.555, 1.760, 1.963, 2.18, 2.38, 2.6 ]
  for (let i = 0; i < 6; ++i) {
    let b = drawWorker((actX[i]) * height,actY,wsize,'cube',color)
    b.style.transform = 'rotate(45deg)'
    dom.appendChild(b)
  }

  let bagY    = height * 0.38
  let bagX    = [ 2.05, 2.26, 2.50, 2.7 ]
  for (let i = 0; i < 4; ++i) {
    let b = drawWorker((bagX[i]) * height,bagY,wsize,'cube',color)
    b.style.transform = 'rotate(45deg)'
    dom.appendChild(b)
  }







  return dom
}


