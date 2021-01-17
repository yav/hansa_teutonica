module Common.Interact
  ( -- * InteractionState
    InteractState
  , startGame
  , GameInfo(..)
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
import Data.List(foldl')
import Control.Monad(liftM,ap)
import GHC.Generics(Generic)

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:?))
import qualified Data.Aeson as JS

import Common.Basics
import AppTypes(State,Finished,Input,Update,doUpdate)




startGame :: GameInfo -> [WithPlayer Input] -> InteractState
startGame ginfo moves = foldl' step state0 moves
  where
  state0 = interaction_ (Left (gInit ginfo))
           InteractState { iInit    = ginfo
                         , iLog     = []
                         , iName    = 0
                         , iGame    = Right (gState ginfo)
                         , iAsk     = Map.empty
                         }
  step state i = interaction_ (Right i) state

data GameInfo = GameInfo
  { gPlayers :: Set PlayerId
  , gState   :: State
  , gInit    :: Interact ()
  }


data OutMsg =
    CurGameState InteractState
  | AskQuestions [ChoiceHelp]
  | GameUpdate Update
    deriving Generic

data ChoiceHelp = ChoiceHelp
  { chChoice    :: Input
  , chHelp      :: Text
  , chStateName :: Int    -- ties question to state
  } deriving Generic

data PlayerRequest =
    Reload
  | Undo
  | PlayerResponse ChoiceHelp


handleMessage ::
  WithPlayer PlayerRequest ->
  InteractState -> (InteractState, [WithPlayer OutMsg])
handleMessage (p :-> req) =
  case req of
    Reload -> \s -> (s, [reload s p])

    Undo -> \s ->
      case iLog s of
        (q :-> _) : more | p == q ->
           let s1 = startGame (iInit s) (reverse more)
           in (s1, map (reload s1) (iPlayers s1))
        _ -> (s,[])

    PlayerResponse ch ->
      \s -> if chStateName ch == iName s
              then askQuestions (interaction (Right (p :-> chChoice ch)) s)
              else (s,[])


  where
  askQuestions (s,os) =
    ( s
    , reverse os ++
      [ q :-> AskQuestions qs
      | (q,qs) <- Map.toList
                $ Map.fromListWith (++)
                  [ (q,[ChoiceHelp { chChoice = ch
                                   , chHelp = help
                                   , chStateName = iName s
                                   }])
                      | (q :-> ch,(help,_))  <- Map.toList (iAsk s) ]
      ]
    )


reload :: InteractState -> PlayerId -> WithPlayer OutMsg
reload s p =
  let forMe (q :-> _) _ = p == q
      ps = s { iAsk = Map.filterWithKey forMe (iAsk s) }
  in p :-> CurGameState ps



data InteractState =
  InteractState
    { iInit   :: GameInfo

    , iLog    :: [WithPlayer Input]
      -- ^ A record of all responses made by the players

    , iName   :: !Int

    , iGame   :: Either Finished State
      -- ^ The current game state.
      -- Should be reproducable by replyaing the log file on the initial state

    , iAsk     :: Map (WithPlayer Input) (Text, R)
      -- ^ Choices avialable to the players.
    }

iPlayers :: InteractState -> [PlayerId]
iPlayers = Set.toList . gPlayers . iInit


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
  Either (Interact ()) (WithPlayer Input) ->
  InteractState -> (InteractState, [WithPlayer OutMsg])
interaction how s = (s1,msgs)
  where
  Interact m =
     case how of
       Left m'    -> m'
       Right resp -> continueWith resp

  (s1,os) = m (\_ -> (,)) s []
  msgs    = [ p :-> o | p <- iPlayers s1
                      , o <- map GameUpdate os ]

interaction_ :: Either (Interact ()) (WithPlayer Input) ->
               InteractState -> InteractState
interaction_ how = fst . interaction how

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
continueWith :: WithPlayer Input -> Interact a
continueWith msg = Interact $
  \_ ->
  \s os ->
    case Map.lookup msg (iAsk s) of
      Just (_,f)  ->
        let s1 = s { iAsk = Map.empty
                   , iLog = msg : iLog s, iName = iName s + 1 }
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



instance ToJSON OutMsg

instance ToJSON InteractState where
  toJSON g =
    JS.object
      [ case iGame g of
          Right a -> "game" .= a
          Left a  -> "finished" .= a
      , "questions" .= toChoiceHelp g
      ]
    where
    toChoiceHelp :: InteractState -> [ ChoiceHelp ]
    toChoiceHelp s =
      [ ChoiceHelp { chChoice = q, chHelp = help, chStateName = iName s }
      | (_ :-> q,(help,_)) <- Map.toList (iAsk s) ]

instance FromJSON PlayerRequest where
  parseJSON =
    JS.withObject "request" \o ->
    do tag <- o .:? "tag"
       case tag :: Maybe Text of
         Just "reload" -> pure Reload
         Just "undo"   -> pure Undo
         _ -> PlayerResponse <$> parseJSON (JS.Object o)


instance ToJSON ChoiceHelp
instance FromJSON ChoiceHelp

