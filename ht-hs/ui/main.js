let gui

function uiDrawBoard(ws,board) {
  const body = document.getElementById('main')
  const state = sample()
  board.size = 700
  gui = drawBoardIn(body,board)
}

function sample() {
  const redCube = { owner: 'red', shape: 'cube' }
  const purpleCube = { owner: 'purple', shape: 'cube' }
  const greenDisc = { owner: 'green', shape: 'disc' }
  const board = { size: 700
                , map: 'britannia_45'
                , full: 5
                , fullMax: 8
                , nodes: { 1:
                           { annex: [redCube,redCube]
                           , office: [redCube,redCube]
                           }
                         }
                , edges: { 17: { bonus: 'act_3'
                               , workers: { 0: redCube, 2:purpleCube }
                               }
                         }
                , endVP: { 2: 'green' }
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
  srvConnect('ws://0.0.0.0:8000')

  const body = document.getElementById('main')
  const state = sample()
  gui = drawBoardIn(body,state.board)
  gui.placeWorkerInOffice(23,{owner: 'purple', shape: 'cube' })
  const purpleDisc = {owner:'purple', shape: 'disc'}
  const greenCube = {owner:'green', shape: 'cube'}
/*
  ui.askWorkerOnVP(0, purpleDisc)
  ui.askFullEdgeSpot(17,0)
  ui.askFullEdgeSpot(17,1)
  ui.askBonus(17)
  ui.askAnnex(23,purpleDisc)
  ui.askFullOffice(23,0)
  ui.askEmptyOffice(23,greenCube)
  ui.removeBonus(17)
  ui.removeWorkerFromEdge(17,1)
  ui.askUpgrade('actions')
  ui.askUpgrade('privilege')
  for (let i = 0; i < 15; ++i) ui.askEmptyEdgeSpot(i,0,purpleDisc)
*/

  for (const i in state.players) {
    body.appendChild(drawPlayer(state.players[i]))
  }

}
