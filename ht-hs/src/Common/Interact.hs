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


startGame :: Set PlayerId -> app -> Interact app () -> InteractState app
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


data OutMsg app =
    CurGameState (InteractState app)
  | AskQuestions [ChoiceHelp app]
  | GameUpdate (Update app)

data ChoiceHelp app = ChoiceHelp
  { chChoice :: Input app
  , chHelp   :: Text
  }

data PlayerRequest app =
    Reload
  | PlayerResponse (Input app)


handleMessage ::
  App app =>
  WithPlayer (PlayerRequest app) ->
  InteractState app -> (InteractState app, [WithPlayer (OutMsg app)])
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


data InteractState app =
  InteractState
    { iPlayers :: Set PlayerId

    , iGame0  :: app
      -- ^ Initial game state

    , iLog    :: [WithPlayer (Input app)]
      -- ^ A record of all responses made by the players

    , iGame   :: app
      -- ^ The current game state.
      -- Should be reproducable by replyaing the log file on the initial state

    , iAsk     :: Map (WithPlayer (Input app)) (Text, R app)
      -- ^ Choices avialable to the players.
    }


newtype Interact app a = Interact ((a -> R app) -> R app)

instance Functor (Interact app) where
  fmap = liftM

instance Applicative (Interact app) where
  pure a = Interact \k -> k a
  (<*>)  = ap

instance Monad (Interact app) where
  Interact m >>= f = Interact \k -> m \a -> let Interact m1 = f a
                                            in m1 k

type R app =
  InteractState app -> [Update app] -> (InteractState app, [Update app])

-- | Perform an interaction
interaction ::
  Interact app () ->
  InteractState app -> (InteractState app, [WithPlayer (OutMsg app)])
interaction (Interact m) s = (s1,msgs)
  where
  (s1,os) = m (\_ -> (,)) s []
  msgs    = [ p :-> GameUpdate o | p <- Set.toList (iPlayers s1), o <- os ]

choose :: App app => PlayerId -> [(Input app,Text)] -> Interact app (Input app)
choose playerId opts =
  askInputs [ (playerId :-> ch, help, pure ch) | (ch,help) <- opts ]

askInputs ::
  App app =>
  [ (WithPlayer (Input app), Text, Interact app a) ] -> Interact app a
askInputs opts =
  Interact $
  \curK ->
  \curS os ->
  let cont (ch,help,Interact m) = (ch, (help, m curK))
  in (curS { iAsk = Map.union (Map.fromList (map cont opts)) (iAsk curS) }, os)

-- | Resume execution based on player input
continueWith :: App app => WithPlayer (Input app) -> Interact app a
continueWith msg = Interact $
  \_ ->
  \s os ->
    case Map.lookup msg (iAsk s) of
      Just (_,f)  ->
        let s1 = s { iAsk = Map.empty, iLog = msg : iLog s }
        in f s1 os
      Nothing -> (s,os)

-- | Access a component of the current game state
view :: (app -> a) -> Interact app a
view f = Interact $
  \k ->
  \s os -> k (f (iGame s)) s os

-- | Access the current game state
getGameState :: Interact app app
getGameState = view id

-- | Update the current game state
update :: App app => Update app -> Interact app ()
update o = Interact $
  \k    ->
  \s os -> k () s { iGame = doUpdate o (iGame s) } (o : os)




--------------------------------------------------------------------------------
-- Input and output messages



instance App app => ToJSON (OutMsg app) where
  toJSON msg =
    case msg of
      GameUpdate upd  -> toJSON upd
      AskQuestions qs -> jsCall "ask" [qs]
      CurGameState s  -> jsCall "redraw" [s]

instance App app => ToJSON (InteractState app) where
  toJSON g =
    JS.object
      [ "game"      .= iGame g
      , "questions" .= toChoiceHelp g
      , "log"       .= iLog g
      ]
    where
    toChoiceHelp :: InteractState app -> [ ChoiceHelp app ]
    toChoiceHelp s =
      [ ChoiceHelp { chChoice = q, chHelp = help }
      | (_ :-> q,(help,_)) <- Map.toList (iAsk s) ]

instance App app => FromJSON (PlayerRequest app) where
  parseJSON =
    JS.withObject "request" \o ->
    do tag <- o .: "tag"
       case tag :: Text of
         "reload" -> pure Reload
         _        -> PlayerResponse <$> parseJSON (JS.Object o)


instance App app => ToJSON (ChoiceHelp app) where
  toJSON ch = JS.object [ "help" .= chHelp ch, "choice" .= chChoice ch ]

