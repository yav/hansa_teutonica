module Interact
  ( -- * InteractionState
    InteractState
  , startState
  , interaction

  -- * Building Interactions
  , Interact
  , onInput
  , output
  , game
  , game_
  , gameView
  , gameGet

  -- * XXX
  , handleMessage
  , InMsg(..)
  , SystemInMsg(..)
  , OutMsg(..)
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(liftM,ap)

import qualified Data.Aeson as JS
import Data.Aeson ((.=))

import Basics
import Question
import Game


data InteractState =
  InteractState
    { iGame :: Game
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

getOutput :: InteractState -> ([WithPlayer OutMsg],InteractState)
getOutput i = (iSay i, i { iSay = [] })

continueWith :: WithPlayer Choice -> InteractState -> InteractState
continueWith ch s =
  case Map.lookup ch (iAsk s) of
    Just s1 -> s1
    Nothing -> s



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



onInput :: WithPlayer Choice -> Interact a -> Interact a
onInput ch (Interact k) = Interact \curK ->
  \curS ->
    let cont = k curK curS { iLog = ch : iLog curS
                           , iAsk = Map.empty
                           }
    in curS { iAsk = Map.insert ch cont (iAsk curS) }

output :: WithPlayer OutMsg -> Interact ()
output m = Interact \k s -> k () s { iSay = m : iSay s }

interaction :: Interact () -> InteractState -> InteractState
interaction (Interact k) = k \_ -> id

game :: (Game -> (a,Game)) -> Interact a
game f = Interact \k s -> case f (iGame s) of
                            (a,g1) -> k a s { iGame = g1 }

game_ :: (Game -> Game) -> Interact ()
game_ f = game \g -> ((), f g)

gameView :: (Game -> a) -> Interact a
gameView f = game \g -> (f g, g)

gameGet :: Interact Game
gameGet = gameView id


--------------------------------------------------------------------------------

handleMessage ::
  WithPlayer InMsg -> InteractState -> (InteractState, [WithPlayer OutMsg])
handleMessage (player :-> msg) s =
  case msg of
    External ch ->
      let (out,s1) = getOutput (continueWith (player :-> ch) s)
      in (s1,out)

    System {} -> error "XXX"



data SystemInMsg =
    Disconnected
  | Connected

data InMsg = System SystemInMsg | External Choice

data OutMsg = PlaceWorkerOnEdge EdgeId Worker



--------------------------------------------------------------------------------
instance JS.ToJSON OutMsg where
  toJSON = undefined



