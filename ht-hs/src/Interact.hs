module Interact
  ( -- * InteractionState
    InteractState
  , startState
  , interaction

  -- * Building Interactions
  , Interact
  , askInputs
  , choose
  , inGame
  , viewGame
  , update

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
                     (Text, InteractState Updates -> InteractState Updates)
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


choose :: PlayerId -> [(Choice,Text)] -> Interact Choice
choose playerId opts =
  askInputs [ (playerId :-> ch, help, pure ch) | (ch,help) <- opts ]

askInputs :: [ (WithPlayer Choice, Text, Interact a) ] -> Interact a
askInputs opts = Interact \curK curS ->
  let cont (ch,help,Interact m) = (ch, (help, m curK))
  in curS { iAsk = Map.fromList (map cont opts) }

-- | Resume execution based on player input
continueWith ::
  WithPlayer Choice -> Interact a
continueWith msg = Interact \_ s ->
  case Map.lookup msg (iAsk s) of
    Just (_,f)  -> f s { iAsk = Map.empty, iLog = msg : iLog s }
    Nothing     -> s

-- | Do something with the game state
inGame :: Game a -> Interact a
inGame g = Interact \k s -> case runGame g (iGame s) of
                              (a,g1) -> k a s { iGame = g1 }

viewGame :: (GameState NoUpdates -> b) -> Interact b
viewGame f = inGame (view f)

update :: GameUpdate -> Interact ()
update = inGame . gameUpdate


newtype InteractBuilder a = IB (Interact a)
  deriving (Functor,Applicative,Monad)


--------------------------------------------------------------------------------


data OutMsg =
    CurGameState (InteractState NoUpdates)
  | AskQuestions [ChoiceHelp]
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
  toJSON g =
    JS.object
      [ "game"      .= iGame g
      , "questions" .= [ ChoiceHelp { chChoice = q
                                    , chHelp = help }
                       | (_ :-> q,(help,_)) <- Map.toList (iAsk g) ]
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
  case req of
    Reload -> \s ->
                let forMe (q :-> _) _ = p == q
                    ps = s { iAsk = Map.filterWithKey forMe (iAsk s) }
                in (s, [p :-> CurGameState ps])
    PlayerResponse ch ->
      askQuestions .
      interaction (continueWith (p :-> ch))

  where
  askQuestions (s,os) =
    ( s
    , os ++
      [ q :-> AskQuestions qs
      | (q,qs) <- Map.toList
                $ Map.fromListWith (++)
                  [ (q,[ChoiceHelp { chChoice = ch
                                   , chHelp = help
                                   }])
                      | (q :-> ch,(help,_))  <- Map.toList (iAsk s) ]
      ]
    )


data ChoiceHelp = ChoiceHelp
  { chChoice :: Choice
  , chHelp   :: Text
  } deriving (Eq,Ord)

instance JS.ToJSON ChoiceHelp where
  toJSON ch = JS.object [ "help" .= chHelp ch, "choice" .= chChoice ch ]


