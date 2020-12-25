module Game
  ( GameState(..), Updates, NoUpdates
  , initialGameState
  , startInteract
  , endInteract

    -- * Game State Manipulation
  , Game
  , runGame
  , view
  , update
  , GameUpdate(..)
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


newtype Game a = Game (GameState Updates -> (a,GameState Updates))

data GameUpdate =
    PlaceWorkerOnEdge EdgeId Worker
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

output :: WithPlayer GameUpdate -> Game ()
output m = doUpdate \s -> s { gameOutput = m : gameOutput s }

update :: GameUpdate -> Game ()
update = undefined

view :: (GameState NoUpdates -> a) -> Game a
view f = Game \s -> (f s { gameOutput = () }, s)

broadcast :: GameUpdate -> Game ()
broadcast m =
  do ps <- view gameTurnOrder
     forM_ ps \p -> output (p :-> m)

data GameStatus =
    GameInProgress Turn
  | GameFinished Board
    deriving Show

data Turn = Turn
  { turnCurrentPlayer :: PlayerId
  , turnActionsDone   :: Int
  , turnActionLimit   :: Int
  , turnUsedGateways  :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  } deriving Show

newTurn :: PlayerId -> Int -> Turn
newTurn playerId actNum =
  Turn
    { turnCurrentPlayer = playerId
    , turnActionsDone   = 0
    , turnActionLimit   = actNum
    , turnUsedGateways  = Set.empty
    , turnPlaceBonus    = []
    }



initialGameState :: TFGen -> Board -> Set PlayerId -> GameState NoUpdates
initialGameState rng0 board playerIds =
  GameState
    { gamePlayers    = playerState
    , gameTurnOrder  = playerOrder
    , gameTokens     = tokens
    , gameStatus =
        if not (null playerOrder)
          then GameInProgress
                    (newTurn firstPlayer (getLevel firstPlayerState Actions))
          else GameFinished board
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
    , "status"    .= gameStatus g
    ]

instance JS.ToJSON GameStatus where
  toJSON = undefined


instance JS.ToJSON Turn where
  toJSON t = JS.object
    [ "player"   .= turnCurrentPlayer t
    , "actDone"  .= turnActionsDone t
    , "actLimit" .= turnActionLimit t
    , "bonuses"  .= length (turnPlaceBonus t)
    ]


--------------------------------------------------------------------------------
instance JS.ToJSON GameUpdate where
  toJSON = undefined


