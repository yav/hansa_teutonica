{-# Language TemplateHaskell #-}
module Game
  ( Game, GameFinished, GameStatus
  , initialGame
  , playerAfter
  , gameTurnOrder
  , gameCurrentPlayer
  , gamePlayer
  , gameBoard
  , gameTokens
  , gameTurn
  , gameEndVPSpot
  , gameTokenRemaining
  , GameUpdate(..)
  , doUpdate
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(isJust)

import qualified Data.Aeson as JS
import Data.Aeson ((.=),ToJSON(..))
import System.Random.TF(TFGen)

import Common.Utils
import Common.Basics
import Common.Field

import Basics
import Stats
import Bonus
import Player
import Board
import Edge
import Node
import Turn
import Event

data GameUpdate =
    PlaceWorkerOnEdge EdgeId Int Worker
  | RemoveWorkerFromEdge EdgeId Int
  | EdgeRemoveBonus EdgeId
  | EdgeSetBonus EdgeId BonusToken
  | SetEndVPAt Level Worker

  | PlaceWorkerInOffice NodeId Worker

  | SetWorkerPreference Worker
  | ChangeAvailble Worker Int
  | ChangeUnavailable Worker Int
  | ChangeVP PlayerId Int
  | Upgrade PlayerId Stat
  | GainBonusToken PlayerId BonusToken

  | NewTurn Turn
  | ChangeDoneActions Int
  | ChangeActionLimit Int
  | AddWorkerToHand (Maybe ProvinceId) Worker
  | RemoveWokerFromHand
  | UseGateway ProvinceId
  | DrawBonusToken
  | PlacingBonus (Maybe BonusToken)

  | Log Event
  deriving Show

data GameStatus s = Game
  { _gamePlayers  :: Map PlayerId Player
  , gameTurnOrder :: [PlayerId]
  , _gameTokens   :: [BonusToken]
  , _gameTokenRemaining :: Int
  , _gameBoard    :: Board
  , _gameLog      :: [Event]
  , _gameStatus   :: s
  , _gameEndVPSpots :: Map Level Worker
  } deriving (Read,Show)


declareFields ''GameStatus

type Game = GameStatus Turn
type GameFinished = GameStatus FinalScore

data FinalScore = FinalScore -- XXX

gamePlayer :: PlayerId -> Field Game Player
gamePlayer playerId = gamePlayers .> mapAt playerId

gameTurn :: Field Game Turn
gameTurn = gameStatus

gameCurrentPlayer :: Game -> PlayerId
gameCurrentPlayer = currentPlayer . getField gameTurn

gameEndVPSpot :: Level -> Game -> Maybe Worker
gameEndVPSpot lvl = getField (gameEndVPSpots .> mapAtMaybe lvl)

doUpdate :: GameUpdate -> Game -> Either GameFinished Game
doUpdate upd =
  case upd of

    -- Player

    SetWorkerPreference Worker {..} ->
      Right .
        (gamePlayer workerOwner `updField` setWorkerPreference workerType)

    ChangeAvailble Worker{..} n ->
      Right .
        (gamePlayer workerOwner `updField` changeAvailable workerType n)

    ChangeUnavailable Worker{..} n ->
      Right .
        (gamePlayer workerOwner `updField` changeUnavailable workerType n)

    ChangeVP playerId n ->
      Right . (gamePlayer playerId `updField` addVP n)

    Upgrade playerId act ->
      Right . (gamePlayer playerId `updField` levelUp act)

    GainBonusToken playerId bonus ->
      Right . (gamePlayer playerId `updField` gainBonus bonus)

    -- nodes
    PlaceWorkerInOffice nodeId worker ->
      Right .
      (gameBoard .> boardNode nodeId `updField` nodeAddWorker worker)

    SetEndVPAt lvl worker ->
      Right .
        (setField (gameEndVPSpots .> mapAtMaybe lvl) (Just worker))

    -- edges

    PlaceWorkerOnEdge edgeId spot w ->
      Right .
        (gameBoard .> boardEdge edgeId `updField` edgeSetWorker spot (Just w))

    RemoveWorkerFromEdge edgeId spot ->
      Right .
        (gameBoard .> boardEdge edgeId `updField` edgeSetWorker spot Nothing)

    EdgeRemoveBonus edgeId ->
      Right .
        (gameBoard .> boardEdge edgeId `updField` edgeRemoveBonus)

    EdgeSetBonus edgeId bonus ->
      Right .
        (gameBoard .> boardEdge edgeId `updField` edgeSetBonus bonus)


    -- turn

    NewTurn turn -> Right . setField gameTurn turn

    UseGateway g ->
      Right . (gameTurn `updField` useGateway g)

    ChangeDoneActions n ->
      Right . (gameTurn .> actionsDone `updField` (+n))

    ChangeActionLimit n ->
      Right . (gameTurn .> currentActionLimit `updField` (+n))

    AddWorkerToHand prov w ->
      Right . (gameTurn `updField` addWorkerToHand prov w)

    RemoveWokerFromHand ->
      Right . (gameTurn `updField` removeWokerFromHand)

    DrawBonusToken ->
      Right . (gameTokenRemaining `updField` subtract 1)

    PlacingBonus b ->
      Right . (if isJust b then (gameTokens `updField` drop 1) else id)
            . (gameTurn .> turnPlacing   `setField` b)

    -- events
    Log e ->
      Right . (gameLog `updField` (e:))

playerAfter :: PlayerId -> Game -> PlayerId
playerAfter playerId state =
  case break (== playerId) (gameTurnOrder state) of
    (_, _ : next : _) -> next
    (next : _, _)     -> next
    _                 -> playerId -- shouldn't happen



initialGame :: TFGen -> Board -> Set PlayerId -> Game
initialGame rng0 board playerIds =
  Game
    { _gamePlayers   = playerState
    , gameTurnOrder  = playerOrder
    , _gameTokens    = otherTokens
    , _gameTokenRemaining = length otherTokens
    , _gameBoard     = foldr addToken board startToks
    , _gameStatus    = newTurn firstPlayer (getLevel Actions firstPlayerState)
    , _gameLog       = [StartTurn firstPlayer]
    , _gameEndVPSpots= Map.empty
    }

  where
  (playerOrder,initialToks,otherTokens) =
    let (ps, rng1)    = shuffle (Set.toList playerIds) rng0
        (start, rng2) = shuffle startTokens rng1
        (toks,_)      = shuffle tokenList rng2
    in (ps,start,toks)

  startToks = zip initialToks (Set.toList (boardInitialTokens board))
  addToken (tok,loc) = updField (boardEdge loc) (edgeSetBonus tok)

  firstPlayer = head playerOrder
  firstPlayerState = playerState Map.! firstPlayer

  playerState =
    Map.fromList [ (p, initialPlayer i) | p <- playerOrder | i <- [ 0 .. ] ]



--------------------------------------------------------------------------------

instance ToJSON status => ToJSON (GameStatus status) where
  toJSON g = JS.object
    [ "players" .=
        JS.object [ jsKey pId .= p
                  | (pId,p) <- Map.toList (getField gamePlayers g)
                  ]
    , "turnOrder" .= gameTurnOrder g
    , "board"     .= getField gameBoard g
    , "endVP"     .= jsMap (getField gameEndVPSpots g)
    , "log"       .= getField gameLog g
    , "tokens"    .= getField gameTokenRemaining g
    , "status"    .= getField gameStatus g
    ]


instance ToJSON FinalScore where
  toJSON FinalScore = JS.object
    [ jsTag "finished"
    , "score" .= JS.Null -- XXX
    ]

instance ToJSON GameUpdate where
  toJSON upd =
    case upd of
      SetWorkerPreference w    -> jsCall "setWorkerPreference" [w]
      ChangeAvailble a b       -> jsCall "changeAvailable" [js a, js b]
      ChangeUnavailable a b    -> jsCall "changeUnavailable" [js a, js b]
      ChangeVP a b             -> jsCall "changeVP" [js a, js b ]
      Upgrade a b              -> jsCall "upgrade" [js a, js b]
      GainBonusToken a b       -> jsCall "gainBonusToken" [ js a, js b ]

      PlaceWorkerInOffice a b  -> jsCall "placeWorkerInOffice" [ js a, js b ]
      SetEndVPAt a b           -> jsCall "setEndVP" [ js a, js b ]

      PlaceWorkerOnEdge a b c  -> jsCall "setWorkerOnEdge" [js a, js b, js c]
      RemoveWorkerFromEdge a b -> jsCall "removeWorkerFromEdge" [ js a, js b]
      EdgeRemoveBonus a        -> jsCall "edgeRemoveBonus" [a]
      EdgeSetBonus a b         -> jsCall "edgeSetBonus" [js a, js b]

      NewTurn t                -> jsCall "newTurn" [t]
      UseGateway g             -> jsCall "useGateway" [g]
      ChangeDoneActions n      -> jsCall "changeDoneActions" [n]
      ChangeActionLimit n      -> jsCall "changeActionLimit" [n]
      AddWorkerToHand _ w      -> jsCall "addWorkerToHand" [w]
      RemoveWokerFromHand      -> jsCall' "removeWokerFromHand"
      DrawBonusToken           -> jsCall' "drawBonusToken"
      PlacingBonus b           -> jsCall  "placingBonus" [b]
      Log e                    -> jsCall "log" [e]

