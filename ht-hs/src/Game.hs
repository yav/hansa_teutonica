module Game
  ( GameState(..)
  , initialGameStateFromArgs
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
import Text.Read(readMaybe)
import qualified Data.Text as Text
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
import Board.Index(boards)


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
  , gameTokens    :: [BonusToken]
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

newTurn :: PlayerColor -> Int -> Turn
newTurn playerId actNum =
  Turn
    { turnCurrentPlayer = playerId
    , turnActionsDone   = 0
    , turnActionLimit   = actNum
    , turnUsedGateways  = Set.empty
    , turnPlaceBonus    = []
    }



initialGameStateFromArgs :: TFGen -> [String] -> Maybe GameState
initialGameStateFromArgs rng args =
  case args of
    b : rest ->
      do board <- Map.lookup (Text.pack b) boards
         ps    <- mapM readMaybe rest
         pure (initialGameState rng board (Set.fromList ps))
    [] -> Nothing

initialGameState :: TFGen -> Board -> Set PlayerColor -> GameState
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
    , gameOutput     = []
    }

  where
  (playerOrder,tokens) =
    case shuffle (Set.toList playerIds) rng0 of
      (ps, rng1) -> (ps, fst (shuffle tokenList rng1))

  firstPlayer = head playerOrder
  firstPlayerState = playerState Map.! firstPlayer

  playerState =
    Map.fromList [ (p, initialPlayer i) | p <- playerOrder | i <- [ 0 .. ] ]


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


