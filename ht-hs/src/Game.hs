module Game
  ( Game, GameFinished
  , initialGame
  , playerAfter

    -- * Game State Manipulation
  , viewPlayer
  , viewTurn
  , Turn(..)
  , newTurn
  , nextPickedUp
  , GameUpdate(..)
  , GameStatus(..)
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
  { gamePlayers   :: Map PlayerId Player
  , gameTurnOrder :: [PlayerId]
  , gameTokens    :: [BonusToken]
  , gameBoard     :: Board
  , gameStatus    :: s
  } deriving Show

type Game = GameStatus Turn
type GameFinished = GameStatus FinalScore

data FinalScore = FinalScore -- XXX

viewPlayer :: PlayerId -> (Player -> a) -> Game -> a
viewPlayer playerId f = \s -> f (gamePlayers s Map.! playerId)

viewTurn :: (Turn -> a) -> Game -> a
viewTurn f = f . gameStatus

doUpdatePlayer ::
  PlayerId -> (Player -> Player) -> Game -> Game
doUpdatePlayer pid f =
  \s -> s { gamePlayers = Map.adjust f pid (gamePlayers s) }

doUpdateBoard :: (Board -> Board) -> Game -> Game
doUpdateBoard f =
  \s -> s { gameBoard = f (gameBoard s) }

doUpdateTurn :: (Turn -> Turn) -> Game -> Game
doUpdateTurn f = \s -> s { gameStatus = f (gameStatus s) }


doUpdate :: GameUpdate -> Game -> Either GameFinished Game
doUpdate upd =
  case upd of

    SetWorkerPreference w -> Right .
      doUpdatePlayer (workerOwner w) (setWorkerPreference (workerType w))

    ChangeAvailble w n -> Right .
      doUpdatePlayer (workerOwner w) (changeAvailable (workerType w) n)

    ChangeUnavailable w n -> Right .
      doUpdatePlayer (workerOwner w) (changeUnavailable (workerType w) n)


    -- edges

    PlaceWorkerOnEdge edgeId spot w -> Right .
      doUpdateBoard (modifyEdge edgeId (edgeSetWorker spot (Just w)))

    RemoveWorkerFromEdge edgeId spot -> Right .
      doUpdateBoard (modifyEdge edgeId (edgeSetWorker spot Nothing))


    -- turn

    NewTurn turn -> Right .
      doUpdateTurn \_ -> turn

    ChangeDoneActions n -> Right .
       doUpdateTurn \t -> t { turnActionsDone = turnActionsDone t + n }

    ChangeActionLimit n -> Right .
      doUpdateTurn \t -> t { turnActionLimit = turnActionLimit t + n }

    AddWorkerToHand prov w -> Right .
      doUpdateTurn \t -> t { turnPickedUp = (prov,w) : turnPickedUp t }

    RemoveWokerFromHand -> Right .
      doUpdateTurn \t -> t { turnPickedUp = init (turnPickedUp t) }


playerAfter :: PlayerId -> Game -> PlayerId
playerAfter playerId state =
  case break (== playerId) (gameTurnOrder state) of
    (_, _ : next : _) -> next
    (next : _, _)     -> next
    _                 -> playerId -- shouldn't happen



data Turn = Turn
  { turnCurrentPlayer :: PlayerId
  , turnActionsDone   :: Int
  , turnActionLimit   :: Int
  , turnUsedGateways  :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  , turnPickedUp      :: [(Maybe ProvinceId,Worker)]
  } deriving Show

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
    { gamePlayers    = playerState
    , gameTurnOrder  = playerOrder
    , gameTokens     = tokens
    , gameBoard      = board
    , gameStatus     = newTurn firstPlayer (getLevel Actions firstPlayerState)
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
                  | (pId,p) <- Map.toList (gamePlayers g)
                  ]
    , "turnOrder" .= gameTurnOrder g
    , "board"     .= gameBoard g
    , "status"    .= gameStatus g
    ]

instance ToJSON Turn where
  toJSON t = JS.object
    [ jsTag "active"
    , "turn" .= JS.object
                  [ "player"   .= turnCurrentPlayer t
                  , "actDone"  .= turnActionsDone t
                  , "actLimit" .= turnActionLimit t
                  , "bonuses"  .= length (turnPlaceBonus t)
                  , "pickedUp" .= map snd (turnPickedUp t)
                  ]
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

