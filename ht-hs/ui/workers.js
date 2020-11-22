function drawWorker(x,y,size,shape,color) {
  let dom = document.createElement('div')
  dom.classList.add('worker',shape,color)
  let style = dom.style
  if (shape == 'disc') size = size * 1.5
  style.width = size + 'px'
  style.height = size + 'px'
  style.left = x + 'px'
  style.top = y + 'px'
  return dom
}

