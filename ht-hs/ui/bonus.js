function drawBonusToken(x,y,h,b) {
  let dom = document.createElement('img')
  dom.setAttribute('src','img/token/' + b + '.png')
  dom.classList.add('bonus')
  let style = dom.style
  style.left = x + 'px'
  style.top  = y + 'px'
  style.width = h
  style.height = h
  return dom
}
