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
  , getState
  ) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad(liftM,ap)

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?))
import qualified Data.Aeson as JS

import Common.Utils
import Common.Basics
import AppTypes(State,Finished,Input,Update,doUpdate)


startGame :: Set PlayerId -> State -> Interact () -> InteractState
startGame ps g begin =
  fst
  $ interaction (Left begin)
    InteractState
      { iPlayers = ps
      , iGame0   = g
      , iLog     = []
      , iGame    = Right g
      , iAsk     = Map.empty
      }


data OutMsg =
    CurGameState InteractState
  | AskQuestions [ChoiceHelp]
  | LogResponse (WithPlayer ChoiceHelp)
  | GameUpdate Update

data ChoiceHelp = ChoiceHelp
  { chChoice :: Input
  , chHelp   :: Text
  }

data PlayerRequest =
    Reload
  | PlayerResponse ChoiceHelp


handleMessage ::
  WithPlayer PlayerRequest ->
  InteractState -> (InteractState, [WithPlayer OutMsg])
handleMessage (p :-> req) =
  case req of
    Reload -> \s ->
                let forMe (q :-> _) _ = p == q
                    ps = s { iAsk = Map.filterWithKey forMe (iAsk s) }
                in (s, [p :-> CurGameState ps])

    PlayerResponse ch ->
      askQuestions .
      interaction (Right (p :-> ch))


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


data InteractState =
  InteractState
    { iPlayers :: Set PlayerId

    , iGame0  :: State
      -- ^ Initial game state

    , iLog    :: [WithPlayer ChoiceHelp]
      -- ^ A record of all responses made by the players

    , iGame   :: Either Finished State
      -- ^ The current game state.
      -- Should be reproducable by replyaing the log file on the initial state

    , iAsk     :: Map (WithPlayer Input) (Text, R)
      -- ^ Choices avialable to the players.
    }


newtype Interact a = Interact ((a -> R) -> R)

instance Functor Interact where
  fmap = liftM

instance Applicative Interact where
  pure a = Interact \k -> k a
  (<*>)  = ap

instance Monad Interact where
  Interact m >>= f = Interact \k -> m \a -> let Interact m1 = f a
                                            in m1 k

type R = InteractState -> [Update] -> (InteractState, [Update])

-- | Perform an interaction
interaction ::
  Either (Interact ()) (WithPlayer ChoiceHelp) ->
  InteractState -> (InteractState, [WithPlayer OutMsg])
interaction how s = (s1,msgs)
  where
  (Interact m,extLog) =
     case how of
       Left m'    -> (m',[])
       Right resp -> (continueWith resp,[LogResponse resp])

  (s1,os) = m (\_ -> (,)) s []
  msgs    = [ p :-> o | p <- Set.toList (iPlayers s1)
                      , o <- extLog ++ map GameUpdate os ]

choose :: PlayerId -> [(Input,Text)] -> Interact Input
choose playerId opts =
  askInputs [ (playerId :-> ch, help, pure ch) | (ch,help) <- opts ]

askInputs :: [ (WithPlayer Input, Text, Interact a) ] -> Interact a
askInputs opts =
  Interact $
  \curK ->
  \curS os ->
  let cont (ch,help,Interact m) = (ch, (help, m curK))
  in (curS { iAsk = Map.union (Map.fromList (map cont opts)) (iAsk curS) }, os)

-- | Resume execution based on player input
continueWith :: WithPlayer ChoiceHelp -> Interact a
continueWith msg = Interact $
  \_ ->
  \s os ->
    case Map.lookup (chChoice <$> msg) (iAsk s) of
      Just (_,f)  ->
        let s1 = s { iAsk = Map.empty, iLog = msg : iLog s }
        in f s1 os
      Nothing -> (s,os)

-- | Access a component of the current game state
view :: (State -> a) -> Interact a
view f = Interact $
  \k ->
  \s os -> case iGame s of
             Right st -> k (f st) s os
             Left _   -> (s,os)

-- | Access the current game state
getState :: Interact State
getState = view id

-- | Update the current game state
update :: Update -> Interact ()
update o = Interact $
  \k    ->
  \s os -> case iGame s of
             Left _  -> (s,os)
             Right a -> k () s { iGame = doUpdate o a } (o : os)




--------------------------------------------------------------------------------
-- Input and output messages



instance ToJSON OutMsg where
  toJSON msg =
    case msg of
      GameUpdate upd  -> toJSON upd
      AskQuestions qs -> jsCall "ask" [qs]
      LogResponse ch  -> jsCall "log" [ch]
      CurGameState s  -> jsCall "redraw" [s]

instance ToJSON InteractState where
  toJSON g =
    JS.object
      [ case iGame g of
          Right a -> "game" .= a
          Left a  -> "finished" .= a
      , "questions" .= toChoiceHelp g
      , "log"       .= iLog g
      ]
    where
    toChoiceHelp :: InteractState -> [ ChoiceHelp ]
    toChoiceHelp s =
      [ ChoiceHelp { chChoice = q, chHelp = help }
      | (_ :-> q,(help,_)) <- Map.toList (iAsk s) ]

instance FromJSON PlayerRequest where
  parseJSON =
    JS.withObject "request" \o ->
    do tag <- o .:? "tag"
       case tag :: Maybe Text of
         Just "reload" -> pure Reload
         _  -> PlayerResponse <$> parseJSON (JS.Object o)


instance ToJSON ChoiceHelp where
  toJSON ch = JS.object [ "help" .= chHelp ch, "choice" .= chChoice ch ]


instance FromJSON ChoiceHelp where
  parseJSON = JS.withObject "choice help" \o ->
    do help <- o .: "help"
       ch   <- o .: "choice"
       pure ChoiceHelp { chHelp = help, chChoice = ch }

