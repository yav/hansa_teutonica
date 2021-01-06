{-# Language TemplateHaskell #-}
module Game
  ( Game, GameFinished, GameStatus
  , gameTurnOrder
  , gamePlayer
  , gameBoard
  , gameTokens
  , gameTurn

  , initialGame
  , playerAfter

  , GameUpdate(..)
  , doUpdate
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

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
import Turn

data GameUpdate =
    SetWorkerPreference Worker
  | PlaceWorkerOnEdge EdgeId Int Worker
  | RemoveWorkerFromEdge EdgeId Int
  | ChangeAvailble Worker Int
  | ChangeUnavailable Worker Int

  | ChangeDoneActions Int
  | ChangeActionLimit Int
  | AddWorkerToHand (Maybe ProvinceId) Worker
  | RemoveWokerFromHand
  | UseGateway ProvinceId

  | NewTurn Turn
  deriving Show

data GameStatus s = Game
  { _gamePlayers  :: Map PlayerId Player
  , gameTurnOrder :: [PlayerId]
  , _gameTokens   :: [BonusToken]
  , _gameBoard    :: Board
  , _gameStatus   :: s
  } deriving Show


declareFields ''GameStatus

type Game = GameStatus Turn
type GameFinished = GameStatus FinalScore

data FinalScore = FinalScore -- XXX

gamePlayer :: PlayerId -> Field Game Player
gamePlayer playerId = gamePlayers .> mapAt playerId

gameTurn :: Field Game Turn
gameTurn = gameStatus

doUpdate :: GameUpdate -> Game -> Either GameFinished Game
doUpdate upd =
  case upd of

    SetWorkerPreference Worker {..} ->
      Right .
        (gamePlayer workerOwner `updField` setWorkerPreference workerType)

    ChangeAvailble Worker{..} n ->
      Right .
        (gamePlayer workerOwner `updField` changeAvailable workerType n)

    ChangeUnavailable Worker{..} n ->
      Right .
        (gamePlayer workerOwner `updField` changeUnavailable workerType n)


    -- edges

    PlaceWorkerOnEdge edgeId spot w ->
      Right .
        (gameBoard `updField` modifyEdge edgeId (edgeSetWorker spot (Just w)))

    RemoveWorkerFromEdge edgeId spot ->
      Right .
        (gameBoard `updField` modifyEdge edgeId (edgeSetWorker spot Nothing))


    -- turn

    NewTurn turn -> Right . setField gameTurn turn

    UseGateway g ->
      Right . (gameTurn `updField` useGateway g)

    ChangeDoneActions n ->
      Right . (gameTurn `updField` updField actionsDone (+n))

    ChangeActionLimit n ->
      Right . (gameTurn `updField` updField currentActionLimit (+n))

    AddWorkerToHand prov w ->
      Right . (gameTurn `updField` addWorkerToHand prov w)

    RemoveWokerFromHand ->
      Right .  (gameTurn `updField` removeWokerFromHand)


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
    , _gameTokens    = tokens
    , _gameBoard     = board
    , _gameStatus    = newTurn firstPlayer (getLevel Actions firstPlayerState)
    }

  where
  (playerOrder,tokens) =
    case shuffle (Set.toList playerIds) rng0 of
      (ps, rng1) -> (ps, fst (shuffle tokenList rng1))

  firstPlayer = head playerOrder
  firstPlayerState = playerState Map.! firstPlayer

  playerState =
    Map.fromList [ (p, initialPlayer i) | p <- playerOrder | i <- [ 0 .. ] ]



--------------------------------------------------------------------------------

instance ToJSON status => ToJSON (GameStatus status) where
  toJSON g = JS.object
    [ "players" .=
        JS.object [ playerIdToKey pId .= p
                  | (pId,p) <- Map.toList (getField gamePlayers g)
                  ]
    , "turnOrder" .= gameTurnOrder g
    , "board"     .= getField gameBoard g
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

      PlaceWorkerOnEdge a b c  -> jsCall "setWorkerOnEdge" [js a, js b, js c]
      RemoveWorkerFromEdge a b -> jsCall "removeWorkerFromEdge" [ js a, js b]

      NewTurn t                -> jsCall "newTurn" [t]
      UseGateway g             -> jsCall "usedGateway" [g]
      ChangeDoneActions n      -> jsCall "changeDoneActions" [n]
      ChangeActionLimit n      -> jsCall "changeActionLimit" [n]
      AddWorkerToHand _ w      -> jsCall "addWorkerToHand" [w]
      RemoveWokerFromHand      -> jsCall' "removeWokerFromHand"

