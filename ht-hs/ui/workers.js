function drawWorker(size,worker) {
  let dom = document.createElement('div')
  dom.classList.add('worker',worker.shape,worker.owner)
  size = workerSize(size,worker.shape)
  let style = dom.style
  style.width = size + 'px'
  style.height = size + 'px'
  style.fontSize = (size * 0.8) + 'px'
  style.position = 'relative'
  return dom
}

function workerSize(size,shape) {
  return (shape == 'disc') ? size * 1.5 : size
}

function drawWorkerAt(loc,size,worker) {
  let dom = drawWorker(size,worker)
  let style = dom.style
  style.position = 'absolute'
  style.left = loc.x + 'px'
  style.top = loc.y + 'px'
  style.borderWidth = size / 5
  return dom
}





