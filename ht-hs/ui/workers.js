function drawWorker(size,shape,color) {
  let dom = document.createElement('div')
  dom.classList.add('worker',shape,color)
  size = workerSize(size,shape)
  let style = dom.style
  style.width = size + 'px'
  style.height = size + 'px'
  style.fontSize = (size * 0.8) + 'px'
  return dom
}

function workerSize(size,shape) {
  return (shape == 'disc') ? size * 1.5 : size
}

function drawWorkerAt(x,y,size,shape,color) {
  let dom = drawWorker(size,shape,color)
  let style = dom.style
  style.position = 'absolute'
  style.left = x + 'px'
  style.top = y + 'px'
  style.borderWidth = size / 5
  return dom
}





