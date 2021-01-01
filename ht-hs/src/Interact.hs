module Interact
  ( -- * InteractionState
    InteractState
  , startState
  , interaction

  -- * Building Interactions
  , Interact
  , askInputs
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
import Data.Aeson((.:),(.=))
import qualified Data.Aeson as JS

import Utils
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

    , iAsk    :: Map (WithPlayer Choice)
                     (InteractState Updates -> InteractState Updates)
      -- ^ Choices avialable to the players.
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


askInputs :: [ (WithPlayer Choice, Interact ()) ] -> Interact ()
askInputs opts = Interact \curK curS ->
  let cont (ch,Interact m) = (ch, m curK)
  in curS { iAsk = Map.fromList (map cont opts) }

-- | Resume execution based on player input
continueWith ::
  WithPlayer Choice -> Interact a
continueWith msg = Interact \_ s ->
  case Map.lookup msg (iAsk s) of
    Just f  -> f s { iAsk = Map.empty, iLog = msg : iLog s }
    Nothing -> s

-- | Do something with the game state
game :: Game a -> Interact a
game g = Interact \k s -> case runGame g (iGame s) of
                            (a,g1) -> k a s { iGame = g1 }


newtype InteractBuilder a = IB (Interact a)
  deriving (Functor,Applicative,Monad)


--------------------------------------------------------------------------------


data OutMsg =
    CurGameState (InteractState NoUpdates)
  | AskQuestions [Choice]
  | GameUpdate GameUpdate

data PlayerRequest =
    Reload
  | PlayerResponse Choice


instance JS.ToJSON OutMsg where
  toJSON msg =
    case msg of
      GameUpdate upd  -> JS.toJSON upd
      AskQuestions qs -> jsCall "ask" [qs]
      CurGameState s -> jsCall "redraw" [s]

instance JS.ToJSON (InteractState NoUpdates) where
  toJSON g = JS.object [ "game"      .= iGame g
                       , "questions" .= [ q | _ :-> q <- Map.keys (iAsk g) ]
                       , "log"       .= iLog g
                       ]


instance JS.FromJSON PlayerRequest where
  parseJSON = JS.withObject "request" \o ->
              do tag <- o .: "tag"
                 case tag :: Text of
                   "reload" -> pure Reload
                   _        -> PlayerResponse <$> JS.parseJSON (JS.Object o)

startState :: GameState NoUpdates -> InteractState NoUpdates
startState g =
  InteractState
    { iGame0  = g
    , iLog    = []
    , iGame   = g
    , iAsk    = Map.empty
    }


handleMessage ::
  WithPlayer PlayerRequest ->
  InteractState NoUpdates -> (InteractState NoUpdates, [WithPlayer OutMsg])
handleMessage (p :-> req) =
  askQuestions .
  case req of
    Reload -> \s ->
                let forMe (q :-> _) _ = p == q
                    ps = s { iAsk = Map.filterWithKey forMe (iAsk s) }
                in (s, [p :-> CurGameState ps])
    PlayerResponse ch -> interaction (continueWith (p :-> ch))

  where
  askQuestions (s,os) =
    ( s
    , os ++
      [ q :-> AskQuestions qs
      | (q,qs) <- Map.toList $ Map.fromListWith (++)
                                 [ (q,[ch]) | q :-> ch <- Map.keys (iAsk s) ]
      ]
    )





