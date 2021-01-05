{-# Language TemplateHaskell #-}
module Game
  ( Game, GameFinished, GameStatus(..)
  , gamePlayer
  , gameBoard
  , gameTurn

  , initialGame
  , playerAfter

    -- * Game State Manipulation
  , Turn(..)
  , newTurn
  , nextPickedUp
  , GameUpdate(..)
  , doUpdate
  ) where

import Data.Maybe(listToMaybe)
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
import Question

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

  | NewTurn Turn
  deriving Show

data GameStatus s = Game
  { _gamePlayers  :: Map PlayerId Player
  , gameTurnOrder :: [PlayerId]
  , _gameTokens   :: [BonusToken]
  , _gameBoard    :: Board
  , _gameStatus   :: s
  } deriving Show

data Turn = Turn
  { turnCurrentPlayer :: PlayerId
  , turnActionsDone   :: Int
  , turnActionLimit   :: Int
  , turnUsedGateways  :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  , turnPickedUp      :: [(Maybe ProvinceId,Worker)]
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

    ChangeDoneActions n ->
      Right .
        (gameTurn `updField` \t ->
                              t { turnActionsDone = turnActionsDone t + n })

    ChangeActionLimit n -> Right .
      (gameTurn `updField` \t -> t { turnActionLimit = turnActionLimit t + n })

    AddWorkerToHand prov w -> Right .
      (gameTurn `updField` \t -> t { turnPickedUp = (prov,w) : turnPickedUp t })

    RemoveWokerFromHand -> Right .
      (gameTurn `updField` \t -> t { turnPickedUp = init (turnPickedUp t) })


playerAfter :: PlayerId -> Game -> PlayerId
playerAfter playerId state =
  case break (== playerId) (gameTurnOrder state) of
    (_, _ : next : _) -> next
    (next : _, _)     -> next
    _                 -> playerId -- shouldn't happen



newTurn :: PlayerId -> Int -> Turn
newTurn playerId actLvl =
  Turn
    { turnCurrentPlayer = playerId
    , turnActionsDone   = 0
    , turnActionLimit   = actionLimit actLvl
    , turnUsedGateways  = Set.empty
    , turnPlaceBonus    = []
    , turnPickedUp      = []
    }

nextPickedUp :: Turn -> Maybe (Maybe ProvinceId,Worker)
nextPickedUp = listToMaybe  . reverse . turnPickedUp


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

instance ToJSON Turn where
  toJSON t =
    JS.object
      [ "player"   .= turnCurrentPlayer t
      , "actDone"  .= turnActionsDone t
      , "actLimit" .= turnActionLimit t
      , "bonuses"  .= length (turnPlaceBonus t)
      , "pickedUp" .= map snd (turnPickedUp t)
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
      PlaceWorkerOnEdge a b c  -> jsCall "setWorkerOnEdge" [js a, js b, js c]
      RemoveWorkerFromEdge a b -> jsCall "removeWorkerFromEdge" [ js a, js b]
      ChangeAvailble a b       -> jsCall "changeAvailable" [js a, js b]
      ChangeUnavailable a b    -> jsCall "changeUnavailable" [js a, js b]
      ChangeDoneActions n      -> jsCall "changeDoneActions" [n]
      ChangeActionLimit n      -> jsCall "changeActionLimit" [n]
      AddWorkerToHand _ w      -> jsCall "addWorkerToHand" [w]
      RemoveWokerFromHand      -> jsCall' "removeWokerFromHand"
      NewTurn t                -> jsCall "newTurn" [t]

