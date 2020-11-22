function main() {
  let body = document.getElementById('main')
  let ps = [ 'red','blue','green','yellow','purple' ]
  for (let i = 0; i < 5; ++i) {
    let p = drawPlayer(200,ps[i])
    body.appendChild(p)
  }
}
