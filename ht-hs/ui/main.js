function main() {
  let body = document.getElementById('main')
  let ps = [ 'red','blue','green','yellow','purple' ]
  for (let i = 0; i < 5; ++i) {
    let p = drawPlayer( { height: 300
                        , color: ps[i]
                        , book: 1
                        , privilege: 1
                        , actions: 1
                        , keys: 1
                        , bag: 1
                        , available: { cube: 5, disc: 2 }
                        , unavailable: { cube: 3, disc: 0 }
                        , vp: 17
                        , bonuses: { 'act_3': 1
                                   , 'act_4': 2
                                   , 'extra': 3
                                   , 'move': 1
                                   , 'swap': 1
                                   , 'upgrade': 1
                                   }
                        })
    body.appendChild(p)
  }
}
