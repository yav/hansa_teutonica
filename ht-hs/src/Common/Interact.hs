module Common.Interact
  ( -- * InteractionState
    InteractState
  , startGame
  , handleMessage
  , PlayerRequest
  , OutMsg

  -- * Building Interactions
  , Interact
  , askInputs
  , choose
  , view
  , update
  , getGameState
  , GameUpdate(..)
  ) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad(liftM,ap)

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:))
import qualified Data.Aeson as JS

import Common.Utils
import Common.Basics


startGame ::
  Set PlayerId ->
  gs ->
  Interact i o gs () ->
  InteractState i o gs
startGame ps g begin =
  fst
  $ interaction begin
    InteractState
      { iPlayers = ps
      , iGame0   = g
      , iLog     = []
      , iGame    = g
      , iAsk     = Map.empty
      }


data OutMsg i o gs =
    CurGameState (InteractState i o gs)
  | AskQuestions [ChoiceHelp i]
  | GameUpdate o

data ChoiceHelp i = ChoiceHelp
  { chChoice :: i
  , chHelp   :: Text
  } deriving (Eq,Ord)

data PlayerRequest i =
    Reload
  | PlayerResponse i


handleMessage ::
  Ord i =>
  WithPlayer (PlayerRequest i) ->
  InteractState i o gs -> (InteractState i o gs, [WithPlayer (OutMsg i o gs)])
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


data InteractState i o gs =
  InteractState
    { iPlayers :: Set PlayerId

    , iGame0  :: gs
      -- ^ Initial game state

    , iLog    :: [WithPlayer i]
      -- ^ A record of all responses made by the players

    , iGame   :: gs
      -- ^ The current game state.
      -- Should be reproducable by replyaing the log file on the initial state

    , iAsk     :: Map (WithPlayer i) (Text, R i o gs)
      -- ^ Choices avialable to the players.
    }




newtype Interact i o gs a = Interact ((a -> R i o gs) -> R i o gs)

instance Functor (Interact i o gs) where
  fmap = liftM

instance Applicative (Interact i o gs) where
  pure a = Interact \k -> k a
  (<*>)  = ap

instance Monad (Interact i o gs) where
  Interact m >>= f = Interact \k -> m \a -> let Interact m1 = f a
                                            in m1 k

type R i o gs = InteractState i o gs -> [o] -> (InteractState i o gs, [o])

-- | Perform an interaction
interaction ::
  Interact i o gs () ->
  InteractState i o gs -> (InteractState i o gs, [WithPlayer (OutMsg i o gs)])
interaction (Interact m) s = (s1,msgs)
  where
  (s1,os) = m (\_ -> (,)) s []
  msgs    = [ p :-> GameUpdate o | p <- Set.toList (iPlayers s1), o <- os ]

choose :: Ord i => PlayerId -> [(i,Text)] -> Interact i o gs i
choose playerId opts =
  askInputs [ (playerId :-> ch, help, pure ch) | (ch,help) <- opts ]


askInputs ::
  Ord i => [ (WithPlayer i, Text, Interact i o gs a) ] -> Interact i o gs a
askInputs opts = Interact $
  \curK ->
  \curS os ->
  let cont (ch,help,Interact m) = (ch, (help, m curK))
  in (curS { iAsk = Map.union (Map.fromList (map cont opts)) (iAsk curS) }, os)

-- | Resume execution based on player input
continueWith :: Ord i => WithPlayer i -> Interact i o gs a
continueWith msg = Interact $
  \_ ->
  \s os ->
    case Map.lookup msg (iAsk s) of
      Just (_,f)  ->
        let s1 = s { iAsk = Map.empty, iLog = msg : iLog s }
        in f s1 os
      Nothing -> (s,os)

-- | Access a component of the current game state
view :: (gs -> a) -> Interact i o gs a
view f = Interact $
  \k ->
  \s os -> k (f (iGame s)) s os

-- | Access the current game state
getGameState :: Interact i o gs gs
getGameState = view id


class GameUpdate o gs where
  doUpdate :: o -> gs -> gs

-- | Update the current game state
update :: GameUpdate o gs => o -> Interact i o gs ()
update o = Interact $
  \k    ->
  \s os -> k () s { iGame = doUpdate o (iGame s) } (o : os)




--------------------------------------------------------------------------------
-- Input and output messages



instance (ToJSON i, ToJSON o, ToJSON gs) => ToJSON (OutMsg i o gs) where
  toJSON msg =
    case msg of
      GameUpdate upd  -> toJSON upd
      AskQuestions qs -> jsCall "ask" [qs]
      CurGameState s  -> jsCall "redraw" [s]

instance (ToJSON i, ToJSON gs) => ToJSON (InteractState i o gs) where
  toJSON g =
    JS.object
      [ "game"      .= iGame g
      , "questions" .= [ ChoiceHelp { chChoice = q
                                    , chHelp = help }
                       | (_ :-> q,(help,_)) <- Map.toList (iAsk g) ]
      , "log"       .= iLog g
      ]

instance FromJSON i => FromJSON (PlayerRequest i) where
  parseJSON =
    JS.withObject "request" \o ->
    do tag <- o .: "tag"
       case tag :: Text of
         "reload" -> pure Reload
         _        -> PlayerResponse <$> parseJSON (JS.Object o)


instance ToJSON i => ToJSON (ChoiceHelp i) where
  toJSON ch = JS.object [ "help" .= chHelp ch, "choice" .= chChoice ch ]

