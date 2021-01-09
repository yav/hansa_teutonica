function drawBonusToken(h,b) {
  const dom = document.createElement('img')
  dom.setAttribute('src','img/token/' + b + '.png')
  dom.classList.add('bonus')
  const style = dom.style
  style.width = h
  style.height = h
  return dom
}

function drawBonusTokenAt(loc,h,b) {
  const dom = drawBonusToken(h,b)
  const style = dom.style
  style.position = 'absolute'
  style.left = loc.x + 'px'
  style.top  = loc.y + 'px'
  return dom
}

