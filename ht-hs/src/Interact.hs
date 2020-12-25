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
  , PlayerRequest
  , OutMsg
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(liftM,ap)
import Data.Text(Text)
import Data.Aeson((.:))
import qualified Data.Aeson as JS
import Control.Monad(guard)
import Control.Applicative((<|>))

import Basics
import Question
import Game

data InteractState updates =
  InteractState
    { iGame0  :: GameState NoUpdates
      -- ^ Initial game state

    , iLog    :: [WithPlayer Choice]
      -- ^ A record of all responses made by the players

    , iGame   :: GameState updates
      -- ^ The current game state.
      -- Should be reproducable by replyaing the log file on the initial state

    , iAsk    :: Map (WithPlayer Choice) (InteractState Updates)
      -- ^ Choices avialable to the players.
    }

startState :: GameState NoUpdates -> InteractState NoUpdates
startState g =
  InteractState
    { iGame0  = g
    , iLog    = []
    , iGame   = g
    , iAsk    = Map.empty
    }



newtype Interact a = Interact ((a -> Result) -> Result)
type Result        = InteractState Updates -> InteractState Updates

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
  Interact () ->
  InteractState NoUpdates -> (InteractState NoUpdates, [WithPlayer OutMsg])
interaction (Interact k) s =
  let s1      = k (\_ -> id) s { iGame = startInteract (iGame s) }
      (out,g) = endInteract (iGame s1)
  in (s1 { iGame = g }, map (fmap GameUpdate) out)


-- | Ask a question from a specific player
askInput :: PlayerId -> [Choice] -> Interact Choice
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


data OutMsg =
    GameUpdate GameUpdate
  | CurGameState (InteractState NoUpdates)

data PlayerRequest =
    Reload
  | PlayerResponse Choice


instance JS.ToJSON OutMsg where
  toJSON msg =
    case msg of
      GameUpdate upd -> JS.toJSON upd
      CurGameState s -> JS.toJSON s

instance JS.ToJSON (InteractState NoUpdates) where
  toJSON = undefined


instance JS.FromJSON PlayerRequest where
  parseJSON v =  JS.withObject "request" reload v
             <|> (PlayerResponse <$> JS.parseJSON v)
    where
    reload o = do tag <- o .: "tag"
                  guard (tag == ("reload" :: Text))
                  pure Reload

handleMessage ::
  WithPlayer PlayerRequest ->
  InteractState NoUpdates -> (InteractState NoUpdates, [WithPlayer OutMsg])
handleMessage (p :-> req) =
  case req of
    Reload -> \s -> (s, [p :-> CurGameState s])
    PlayerResponse ch -> interaction (continueWith (p :-> ch))






