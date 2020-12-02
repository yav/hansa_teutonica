function main() {

  let body = document.getElementById('main')
  let redCube = { owner: 'red', shape: 'cube' }
  let greenDisc = { owner: 'green', shape: 'disc' }
  let board = drawBoard( { size: null
                         , map: 'britannia-45'
                         , full: 5
                         , fullMax: 10
                         , nodes: { 23:
                                    { annex: [redCube,redCube]
                                    , office: [redCube,redCube]
                                    }
                                  }
                         , edges: { 11: { bonus: 'act_3'
                                          , workers: [ redCube,greenDisc ] } }
                         , endVP: [ null, null, greenDisc, null ]
                         })
  body.appendChild(board)
}

function mainPlayer() {
  let body = document.getElementById('main')
  let ps = [ 'red','blue','green','yellow','purple' ]
  console.log(window.innerHeight)
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
                        , bonuses: { 'extra': 2, 'act_3': 1 }
                        , spentBonuses: 3
                        })
    body.appendChild(p)
  }
}
