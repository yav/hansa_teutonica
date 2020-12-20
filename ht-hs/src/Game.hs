module Game
  ( GameState(..)
  , initialGameState
  , getOutput

    -- * Game State Manipulation
  , Game
  , runGame
  , view
  , update
  , OutMsg(..)
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad(ap,liftM,forM_)

import qualified Data.Aeson as JS
import Data.Aeson ((.=))
import System.Random.TF(TFGen)

import Basics
import Bonus
import Player
import Board


newtype Game a = Game (GameState -> (a,GameState))

data OutMsg =
    PlaceWorkerOnEdge EdgeId Worker
  deriving Show

runGame :: Game a -> GameState -> (a,GameState)
runGame (Game m) s = m s

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


data GameState = GameState
  { gamePlayers   :: Map PlayerColor Player
  , gameTurnOrder :: [PlayerColor]
  , gameStatus    :: GameStatus
  , gameOutput    :: [WithPlayer OutMsg]
  } deriving Show

getOutput :: GameState -> ([WithPlayer OutMsg],GameState)
getOutput i = (gameOutput i, i { gameOutput = [] })

doUpdate :: (GameState -> GameState) -> Game ()
doUpdate f = Game \s -> ((), f s)

output :: WithPlayer OutMsg -> Game ()
output m = doUpdate \s -> s { gameOutput = m : gameOutput s }

update :: OutMsg -> Game ()
update = undefined

view :: (GameState -> a) -> Game a
view f = Game \s -> (f s, s)

broadcast :: OutMsg -> Game ()
broadcast m =
  do ps <- view gameTurnOrder
     forM_ ps \p -> output (p :-> m)

data GameStatus =
    GameInProgress Turn
  | GameFinished Board
    deriving Show

data Turn = Turn
  { turnCurrentPlayer :: PlayerColor
  , turnActionsDone   :: Int
  , turnActionLimit   :: Int
  , turnUsedGateways  :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  } deriving Show



initialGameState :: TFGen -> [String] -> Maybe GameState
initialGameState _ _ =
  Just
    GameState
      { gamePlayers = Map.empty
      , gameTurnOrder = []
      , gameStatus = undefined
      , gameOutput = []
      }



instance JS.ToJSON GameState where
  toJSON g = JS.object
    [ "players" .=
        JS.object [ playerColorToKey pId .= p
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
instance JS.ToJSON OutMsg where
  toJSON = undefined


