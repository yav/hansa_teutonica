function sample() {
  const redCube = { owner: 'red', shape: 'cube' }
  const greenDisc = { owner: 'green', shape: 'disc' }
  const board = { size: 700
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
                }
  const ps = [ 'red','blue','green','yellow','purple' ]
  const players = {}
  for (let i = 0; i < 5; ++i) {
    players[ps[i]] = { height: 120
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
                     }
  }


  return { board: board, players: players }
}

function main() {

  const body = document.getElementById('main')
  const state = sample()
  body.appendChild(drawBoard(state.board))
  for (const i in state.players) {
    body.appendChild(drawPlayer(state.players[i]))
  }

  askEdgeFull(11,0);
}
