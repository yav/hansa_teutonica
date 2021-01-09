module Actions where

import qualified Data.Map as Map
import Control.Monad(guard,msum,forM_)
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Maybe(maybeToList)

import Common.Utils
import Common.Interact
import Common.Field

import Basics
import Stats
import Player
import Board
import Geometry
import Edge
import Node
import Question
import Game
import Turn

nextAction :: Interact ()
nextAction =
  do state <- getState
     -- XXX: check end game
     let normalOpts = tryPlace state ++ tryMove state ++ tryHire state ++
                      tryCompleteEdge state
         -- NOTE: this will not catch the corrner case of the player having
         -- active workers, but there being no place on the board for them.
         opts       = tryEndTurn (null normalOpts) state ++ normalOpts
     askInputs opts
     nextAction

nextTurn :: Interact ()
nextTurn =
  do state <- getState
     let turn = getField gameTurn state
         nextPlayerId = playerAfter (currentPlayer turn) state
         actLvl = getLevel Actions (getField (gamePlayer nextPlayerId) state)
     update (NewTurn (newTurn nextPlayerId actLvl))

-------------------------------------------------------------------------------
startAction :: Game -> [(Turn,Player)]
startAction state =
  do let turn = getField gameTurn state
         playerState = getField (gamePlayer (currentPlayer turn)) state
     guard (getField actionsDone turn < getField currentActionLimit turn)
     pure (turn, playerState)

type PlayerOptions = Game -> [(WithPlayer Choice, Text, Interact ())]

tryEndTurn :: Bool -> PlayerOptions
tryEndTurn forceEnd state =
  do let turn = getField gameTurn state
     guard (forceEnd ||
            getField actionsDone turn == getField currentActionLimit turn)
     pure (currentPlayer turn :-> ChDone "End Turn", "End turn", nextTurn)

tryPlace :: PlayerOptions
tryPlace state =
  do (turn,playerState) <- startAction state
     let workerT      = getWorkerPreference playerState

         board        = getField gameBoard state
         player       = currentPlayer turn

         gateways     = accessibleProvinces player (usedGateways turn) board
         accessible   = maybe True (`Map.member` gateways)
         gatewayFor edgeId =
                        (`Map.lookup` gateways) =<< edgeProvince board edgeId

         totWorkers   = sum (map (`getAvailable` playerState) enumAll)
         canReplace w = workerOwner w /= player &&
                        totWorkers > replacementCost (workerType w)

         getFree      = do guard (getAvailable workerT playerState > 0)
                           x <- freeSpots board accessible workerT
                           pure (x, "Place a worker")
         getFull      = do x <- replaceSpots board accessible workerT canReplace
                           pure (x, "Replace a worker")
         changePref   = do let otherT = otherType workerT
                           guard (getAvailable otherT playerState > 0)
                           let help = "Change preference to " <>
                                      workerTypeToKey otherT
                           pure (ChSetPreference otherT, help)

     (ch,help) <- [ (player :-> c,help) |
                     (c,help) <- changePref ++ getFree ++ getFull ]
     pure (ch, help, handleChoice gatewayFor ch)

  where
  handleChoice gatewayFor (pid :-> ch) =
    case ch of
      ChSetPreference t ->
        do let w = Worker { workerOwner = pid, workerType = t }
           update (SetWorkerPreference w)

      ChEdgeEmpty edgeId spot workerT ->
        do let w = Worker { workerOwner = pid, workerType = workerT }
           update (ChangeAvailble w (-1))
           case gatewayFor edgeId of
             Just g -> update (UseGateway g)
             _      -> pure ()
           update (PlaceWorkerOnEdge edgeId spot w)
           update (ChangeDoneActions 1)

      ChEdgeFull {} -> error "XXX: place full"

      _ -> pure ()



tryMove :: PlayerOptions
tryMove state0 =
  do (turn,playerState) <- startAction state0
     let pieces = movablePieces state0
         limit  = min (movementLimit (getLevel Movement playerState))
                                                          (length pieces)
         player = currentPlayer turn
     guard (limit > 0)
     pickupQuestion (1::Int) player limit pieces
  where
  movablePieces state =
     let board      = getField gameBoard state
         turn       = getField gameTurn state
         player     = currentPlayer turn
         canMove w  = workerOwner w == player
     in pickupSpots board (const True) canMove

  pickupQuestion num player limit opts =
    [ ( player :-> ch
      , "Move worker " <> Text.pack (show num) <> "/" <> Text.pack (show limit)
      , pickup num limit edgeId spot w
      ) | ch@(ChEdgeFull edgeId spot _ w) <- opts ]

  pickup num limit edgeId spot w =
    do update (RemoveWorkerFromEdge edgeId spot)
       prov <- view \g -> edgeProvince (getField gameBoard g) edgeId
       update (AddWorkerToHand prov w)
       opts <- view movablePieces
       if num < limit
         then askInputs $ ( workerOwner w :-> ChDone "Done"
                          , "No more moves"
                          , putDown)
                        : pickupQuestion (num+1) (workerOwner w) limit opts
         else putDown

  putDown =
    do board <- view (getField gameBoard)
       mb    <- view (nextPickedUp . getField gameTurn)
       case mb of
         Nothing -> update (ChangeDoneActions 1)
         Just (thisProv,w) ->
           do let accessible prov = prov == Nothing || prov == thisProv
              ~(ChEdgeEmpty tgtEdge tgtSpot _) <-
                     choose (workerOwner w)
                         do x <- freeSpots board accessible (workerType w)
                            pure (x, "New worker location")

              update RemoveWokerFromHand
              update (PlaceWorkerOnEdge tgtEdge tgtSpot w)
              putDown



tryHire :: PlayerOptions
tryHire state0 =
  do (turn,playerState) <- startAction state0
     let player     = currentPlayer turn
         limit      = hireLimit (getLevel Hire playerState)
         limTxt     = case limit of
                        Nothing -> "Hire all"
                        Just l  -> "Hire 1/" <> Text.pack (show l)
         question t = (player :-> ChPassiveWorker t,
                                  limTxt, hireFirst limit t)
     [ question t | t <- enumAll, getUnavailable t playerState > 0 ]
  where
  hireFirst mbLimit ch =
    do case mbLimit of
         Nothing -> hireAll
         Just limit -> hire 1 ch >> doHire 2 limit
       update (ChangeDoneActions 1)

  hireAll =
    do (_,cubes,discs) <- getWorkers
       hire cubes Cube
       hire discs Disc

  hire n t =
    do playerId <- view (currentPlayer . getField gameTurn)
       let w = Worker { workerOwner = playerId, workerType = t }
       update (ChangeUnavailable w (-n))
       update (ChangeAvailble    w n)

  doHire hiring limit
    | hiring > limit = pure ()
    | otherwise =
      do (playerId,cubes,discs) <- getWorkers
         case 1 + limit - hiring of
           todo
             | cubes == 0 -> hire (min todo discs) Disc
             | discs == 0 -> hire (min todo cubes) Cube
             | cubes + discs <= limit -> hireAll
             | otherwise ->
               do let lab = mconcat ["Hire ", showText hiring, "/",
                                                              showText limit ]
                  x <- choose playerId
                                  [ (ChPassiveWorker t, lab) | t <- enumAll]
                  let ChPassiveWorker ch = x
                  hire 1 ch
                  doHire (1+hiring) limit

  getWorkers =
    do game <- getState
       let playerId = gameCurrentPlayer game
           player   = getField (gamePlayer playerId) game
           cubes    = getUnavailable Cube player
           discs    = getUnavailable Disc player
       pure (playerId,cubes,discs)


tryCompleteEdge :: PlayerOptions
tryCompleteEdge state =
  resolveAmbig
  do (turn,playerState) <- startAction state
     let playerId = currentPlayer turn
         board    = getField gameBoard state
     (edgeId,edgeInfo) <- fullEdgesFor playerId board
     (nodeId,nodeInfo) <- getEdgeNodes edgeId state
     -- XXX: add code to resolve ambiguity in rare case where the
     -- same node is accessible trhough multiple full edges
     concat [ tryJustComplete edgeId playerId
            , tryAnnex node playerState
            , tryOffice nodeId nodeInfo playerState edgeId edgeInfo
            , tryAction node playerState
            ]

  where
  getEdgeNodes :: EdgeId -> Game -> [(NodeId,Node)]
  getEdgeNodes edgeId s =
    let board = getField gameBoard s
        (x,y) = geoEdgeNodes edgeId (boardGeometry board)
    in [ (n, getField (boardNode n) board) | n <- [x,y] ]

  tryAnnex _ _      = [] -- XXX
  tryAction _ _     = [] -- XXX

  giveVPs edgeId =
    do nodes <- view (getEdgeNodes edgeId)
       forM_ nodes \(_,nodeInfo) ->
         case nodeControlledBy nodeInfo of
           Just playerId -> update (ChangeVP playerId 1)
           Nothing -> pure ()

  activateBonus edgeId = pure () -- XXX

  returnWorkers edgeId =
    do workers <- view (edgeWorkers . getField (gameBoard .> boardEdge edgeId))
       forM_ workers \(spotId,_,w) ->
         do update (RemoveWorkerFromEdge edgeId spotId)
            update (ChangeUnavailable w 1)


  tryJustComplete edgeId playerId =
    [ ( playerId :-> ChEdge edgeId
      , "Complete with NO office/action"
      , edgeId
      , do giveVPs edgeId
           activateBonus edgeId
           returnWorkers edgeId
           update (ChangeDoneActions 1)
      )
    ]

  tryOffice nodeId nodeInfo playerState edgeId edgeInfo =
    do spot <- maybeToList (nodeNextFree nodeInfo)
       guard (spotPrivilege spot <= getLevel Privilege playerState)
       (edgeSpotId,worker) <- maybeToList
                $ msum $ map (suitableWorkerFor spot) $ edgeWorkers edgeInfo
       pure ( workerOwner worker :-> ChNodeEmpty nodeId (workerType worker)
            , "Build office"
            , edgeId
            , do giveVPs edgeId
                 update (RemoveWorkerFromEdge edgeId edgeSpotId)
                 update (PlaceWorkerInOffice nodeId worker)
                 activateBonus edgeId
                 returnWorkers edgeId
                 update (ChangeDoneActions 1)
            )


  suitableWorkerFor spot (i,_,w)
    | accepts (spotRequires spot) (workerType w) = Just (i,w)
    | otherwise = Nothing


  resolveAmbig opts =
    Map.elems $
    fmap resolve $
    Map.fromListWith (++) [ (q,[i]) | i@(q,_,_,_) <- opts ]

  resolve opts =
    case opts of
      [(q,h,_,a)] -> (q,h,a)
      _ -> let (q,h,_,_) = head opts
           in (q, h, askInputs [ ( playerAnnot q :-> ChEdge e
                                 , "Comlete this route"
                                 , a
                                 ) | (_,_,e,a) <- opts ])

