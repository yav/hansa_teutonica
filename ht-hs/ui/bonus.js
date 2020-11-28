function drawBonusToken(h,b) {
  let dom = document.createElement('img')
  dom.setAttribute('src','img/token/' + b + '.png')
  dom.classList.add('bonus')
  let style = dom.style
  style.width = h
  style.height = h
  return dom
}

function drawBonusTokenAt(x,y,h,b) {
  let dom = drawBonusToken(h,b)
  let style = dom.style
  style.position = 'absolute'
  style.left = x + 'px'
  style.top  = y + 'px'
  return dom
}
