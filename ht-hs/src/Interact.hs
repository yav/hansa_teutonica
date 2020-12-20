module Interact
  ( -- * InteractionState
    InteractState
  , startState
  , interaction

  -- * Building Interactions
  , Interact
  , askInput
  , game

  -- * XXX
  , handleMessage
  , Choice
  , OutMsg
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(liftM,ap)

import Basics
import Question
import Game


data InteractState =
  InteractState
    { iGame :: GameState
    , iLog  :: [WithPlayer Choice]
    , iAsk  :: Map (WithPlayer Choice) InteractState
    , iSay  :: [WithPlayer OutMsg]
    }

startState :: InteractState
startState =
  InteractState
    { iGame = initialGameState
    , iLog  = []
    , iAsk  = Map.empty
    , iSay  = []
    }



newtype Interact a = Interact ((a -> Result) -> Result)
type Result        = InteractState -> InteractState

instance Functor Interact where
  fmap = liftM

instance Applicative Interact where
  pure a = Interact \k -> k a
  (<*>)  = ap

instance Monad Interact where
  Interact m >>= f = Interact \k -> m \a -> let Interact m2 = f a
                                            in m2 k


-- | Perform an interaction
interaction ::
  Interact () -> InteractState -> (InteractState, [WithPlayer OutMsg])
interaction (Interact k) s =
  let s1      = k (\_ -> id) s
      (out,g) = getOutput (iGame s1)
  in (s1 { iGame = g }, out)


-- | Ask a question from a specific player
askInput :: PlayerColor -> [Choice] -> Interact Choice
askInput p opts = Interact \curK ->
  \curS ->
     let cont ch = Map.insert (p :-> ch)
                     (curK ch curS { iLog = (p :-> ch) : iLog curS
                                   , iAsk = Map.empty })
     in curS { iAsk = foldr cont (iAsk curS) opts }

-- | Resume execution based on player input
continueWith :: WithPlayer Choice -> Interact ()
continueWith msg =
  Interact \_ s -> case Map.lookup msg (iAsk s) of
                     Just s1 -> s1
                     Nothing -> s

-- | Do something with the game state
game :: Game a -> Interact a
game g = Interact \k s -> case runGame g (iGame s) of
                            (a,g1) -> k a s { iGame = g1 }



--------------------------------------------------------------------------------

handleMessage ::
  WithPlayer Choice -> InteractState -> (InteractState, [WithPlayer OutMsg])
handleMessage = interaction . continueWith





