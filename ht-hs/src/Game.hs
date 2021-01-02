module Game
  ( GameState(..), Updates, NoUpdates
  , initialGameState
  , startInteract
  , endInteract

    -- * Game State Manipulation
  , Game
  , runGame
  , view
  , getState
  , gameUpdate
  , Turn(..)
  , GameUpdate(..)
  , GameStatus(..)
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad(ap,liftM,forM_)

import qualified Data.Aeson as JS
import Data.Aeson ((.=))
import System.Random.TF(TFGen)

import Utils
import Basics
import Stats
import Bonus
import Player
import Board
import Edge


newtype Game a = Game (GameState Updates -> (a,GameState Updates))

data GameUpdate =
    SetWorkerPreference Worker
  | PlaceWorkerOnEdge EdgeId Int Worker
  | ChangeAvailble Worker Int
  | ChangeUnavailable Worker Int
  | ChangeDoneActions Int
  | ChangeActionLimit Int
  deriving Show

runGame :: Game a -> GameState Updates -> (a,GameState Updates)
runGame (Game m) s =  m s

instance Functor Game where
  fmap = liftM

instance Applicative Game where
  pure a = Game \s -> (a,s)
  (<*>)  = ap

instance Monad Game where
  Game m >>= f = Game \s -> case m s of
                              (a,s1) ->
                                 let Game m1 = f a
                                 in m1 s1


data GameState updates = GameState
  { gamePlayers   :: Map PlayerId Player
  , gameTurnOrder :: [PlayerId]
  , gameTokens    :: [BonusToken]
  , gameBoard     :: Board
  , gameStatus    :: GameStatus
  , gameOutput    :: updates
  } deriving Show

type Updates   = [WithPlayer GameUpdate]
type NoUpdates = ()

startInteract :: GameState NoUpdates -> GameState Updates
startInteract g = g { gameOutput = [] }

endInteract ::
  GameState Updates -> ([WithPlayer GameUpdate],GameState NoUpdates)
endInteract i = (gameOutput i, i { gameOutput = () })

doUpdate :: (GameState Updates -> GameState Updates) -> Game ()
doUpdate f = Game \s -> ((), f s)

doUpdatePlayer :: PlayerId -> (Player -> Player) -> Game ()
doUpdatePlayer pid f =
  doUpdate \s -> s { gamePlayers = Map.adjust f pid (gamePlayers s) }

doUpdateBoard :: (Board -> Board) -> Game ()
doUpdateBoard f =
  doUpdate \s -> s { gameBoard = f (gameBoard s) }

doUpdateTurn :: (Turn -> Turn) -> Game ()
doUpdateTurn f =
  doUpdate \s -> s { gameStatus = case gameStatus s of
                                    GameInProgress t -> GameInProgress (f t)
                                    _ -> gameStatus s }

gameUpdate :: GameUpdate -> Game ()
gameUpdate upd =
  do case upd of

       SetWorkerPreference w ->
         doUpdatePlayer (workerOwner w) (setWorkerPreference (workerType w))

       ChangeAvailble w n ->
         doUpdatePlayer (workerOwner w) (changeAvailable (workerType w) n)

       ChangeUnavailable w n ->
         doUpdatePlayer (workerOwner w) (changeUnavailable (workerType w) n)

       ChangeDoneActions n ->
          doUpdateTurn \t -> t { turnActionsDone = turnActionsDone t + n }

       ChangeActionLimit n ->
          doUpdateTurn \t -> t { turnActionLimit = turnActionLimit t + n }

       PlaceWorkerOnEdge edgeId spot w ->
         doUpdateBoard $ modifyEdge edgeId $ edgeSetWorker spot $ Just w


     broadcast upd


view :: (GameState NoUpdates -> a) -> Game a
view f = Game \s -> (f s { gameOutput = () }, s)

getState :: Game (GameState NoUpdates)
getState = view id


broadcast :: GameUpdate -> Game ()
broadcast m =
  do ps <- view gameTurnOrder
     forM_ ps \p -> output (p :-> m)

output :: WithPlayer GameUpdate -> Game ()
output m = doUpdate \s -> s { gameOutput = m : gameOutput s }



data GameStatus =
    GameInProgress Turn
  | GameFinished
    deriving Show

data Turn = Turn
  { turnCurrentPlayer :: PlayerId
  , turnActionsDone   :: Int
  , turnActionLimit   :: Int
  , turnUsedGateways  :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  } deriving Show

newTurn :: PlayerId -> Int -> Turn
newTurn playerId actLvl =
  Turn
    { turnCurrentPlayer = playerId
    , turnActionsDone   = 0
    , turnActionLimit   = actionLimit actLvl
    , turnUsedGateways  = Set.empty
    , turnPlaceBonus    = []
    }



initialGameState :: TFGen -> Board -> Set PlayerId -> GameState NoUpdates
initialGameState rng0 board playerIds =
  GameState
    { gamePlayers    = playerState
    , gameTurnOrder  = playerOrder
    , gameTokens     = tokens
    , gameBoard      = board
    , gameStatus =
        if not (null playerOrder)
          then GameInProgress
                    (newTurn firstPlayer (getLevel firstPlayerState Actions))
          else GameFinished
    , gameOutput     = ()
    }

  where
  (playerOrder,tokens) =
    case shuffle (Set.toList playerIds) rng0 of
      (ps, rng1) -> (ps, fst (shuffle tokenList rng1))

  firstPlayer = head playerOrder
  firstPlayerState = playerState Map.! firstPlayer

  playerState =
    Map.fromList [ (p, initialPlayer i) | p <- playerOrder | i <- [ 0 .. ] ]


instance JS.ToJSON (GameState NoUpdates) where
  toJSON g = JS.object
    [ "players" .=
        JS.object [ playerIdToKey pId .= p
                  | (pId,p) <- Map.toList (gamePlayers g)
                  ]
    , "turnOrder" .= gameTurnOrder g
    , "board"     .= gameBoard g
    , "status"    .= gameStatus g
    ]

instance JS.ToJSON GameStatus where
  toJSON gs =
    case gs of
      GameInProgress t ->
        JS.object [ jsTag "active", "turn" .=  t ]
      GameFinished -> JS.object [ jsTag "finished" ]

instance JS.ToJSON Turn where
  toJSON t = JS.object
    [ "player"   .= turnCurrentPlayer t
    , "actDone"  .= turnActionsDone t
    , "actLimit" .= turnActionLimit t
    , "bonuses"  .= length (turnPlaceBonus t)
    ]


--------------------------------------------------------------------------------
instance JS.ToJSON GameUpdate where
  toJSON upd =
    case upd of
      SetWorkerPreference w   -> jsCall "setWorkerPreference" [w]
      PlaceWorkerOnEdge a b c -> jsCall "setWorkerOnEdge" [js a, js b, js c]
      ChangeAvailble a b      -> jsCall "changeAvailable" [js a, js b]
      ChangeUnavailable a b   -> jsCall "changeUnavailable" [js a, js b]
      ChangeDoneActions n     -> jsCall "changeDoneActions" [n]
      ChangeActionLimit n     -> jsCall "changeActionLimit" [n]
 
