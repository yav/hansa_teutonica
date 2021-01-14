function drawLog() {
  const dom = document.createElement('div')
  dom.classList.add('log')
  dom.style.fontSize = 1.5 * gui.board.fontSize
  gui.panel.appendChild(dom)

  let turnBox = null
  let actionBox = null
  let box
  return {
    addLog: function(msg) {

      box = document.createElement('div')
      box.classList.add('log-item')

      const lab = function(x,cl) {
        const el = document.createElement('span')
        el.textContent = x
        if (cl !== undefined)
          for (let i = 0; i < cl.length; ++i)
            el.classList.add(cl[i])
        box.appendChild(el)
      }

      const sayEdge = function(edgeId,spot) {
        const nodes = gui.board.edgeNodes[edgeId]
        const from  = gui.board.nodeNames[nodes[0]]
        const to    = gui.board.nodeNames[nodes[1]]
        let msg = from + '-' + to
        if (spot !== undefined) {
          msg = msg + ', spot ' + spot
        }
        lab(msg,['log-unit'])
      }

      const sayNode = function(nodeId) {
        lab(gui.board.nodeNames[nodeId],['log-unit'])
      }

      const sayWorker = function(worker) {
        box.appendChild(drawWorker(gui.board.workerSize,worker))
      }

      const sayPlayer = function(player) {
        lab(player, [ 'turn-player', gui.colors[player]])
      }

      const sayBonus = function(bonus) {
        box.appendChild(drawBonusToken(gui.board.bonusSize,bonus))
      }

      switch (msg.tag) {
        case 'start-turn': {
          turnBox = document.createElement('div')
          turnBox.classList.add('log-turn')
          dom.prepend(turnBox)
          sayPlayer(msg.player)
          lab('\'s trun')
          break
        }

        case 'end-turn': {
          turnBox = null
          break
        }

        case 'start-action': {
          actionBox = document.createElement('div')
          actionBox.classList.add('log-action')
          const it = turnBox ? turnBox : dom
          it.appendChild(actionBox)
          return
        }

        case 'end-action': {
          actionBox = null
          return
        }

        case 'pick-up': {
          lab('Picked-up ')
          sayWorker(msg.worker)
          lab(' from ')
          sayEdge(msg.edge,msg.spot)
          break
        }

        case 'place-worker': {
          lab('Placed ')
          sayWorker(msg.worker)
          lab(' on ')
          sayEdge(msg.edge,msg.spot)
          break
        }

        case 'move-worker': {
          lab('Moved ')
          sayWorker(msg.worker)
          lab(' to ')
          sayEdge(msg.edge,msg.spot)
          break
        }



        case 'replace-worker': {
          lab('Replaced ')
          sayWorker(msg.workerOld)
          lab(' with ')
          sayWorker(msg.workerNew)
          lab(' on ')
          sayEdge(msg.edge,msg.spot)
          break
        }

        case 'complete-route': { 
          lab('Completed ')
          sayEdge(msg.edge)
          break
        }

        case 'build-office': {
          sayWorker(msg.worker)
          lab (' established office in ')
          sayNode(msg.node)
          break
        }



        case 'hire': {
          if (msg.number == 0) return
          lab('Hired ' + msg.number + ' ')
          sayWorker(msg.worker)
          break
        }

        case 'retire': {
          lab('Laid off ' + msg.number + ' ')
          sayWorker(msg.worker)
          break
        }

        case 'vp': {
          sayPlayer(msg.player)
          lab(' gained ' + msg.vp + ' VP')
          break
        }

       case 'upgrade': {
         lab('Upgraded ' + msg.action)
         break
       }

      case 'invested': {
        lab('Placed ')
        sayWorker(msg.worker)
        lab(' on ')
        sayNode(msg.node)
        lab(' ' + msg.points + ' VP')
        break
      }

      case 'place-bonus': {
        lab('Placed ')
        sayBonus(msg.bonus)
        lab(' on ')
        sayEdge(msg.edge)
        break
      }

      case 'used-bonus': {
        lab('Used ')
        sayBonus(msg.bonus)
        break
      }

      case 'used-bonus-swap': {
        lab('Used ')
        sayBonus(msg.bonus)
        lab(' on ')
        sayNode(msg.node)
        lab(', spot ' + msg.spot)
        break
      }



        default:
          lab(JSON.stringify(msg))
      }

      { const el = actionBox ? actionBox
                 : (turnBox   ? turnBox : dom)
        el.appendChild(box)
      }
    }
  }
}


