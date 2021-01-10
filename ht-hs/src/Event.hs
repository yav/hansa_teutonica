module Event where

import Data.Aeson(ToJSON,(.=))
import qualified Data.Aeson as JS

import Common.Utils

import Basics

-- User readable events desribing the game flow
data Event =
    StartTurn PlayerId
  | EndTurn PlayerId
  | StartAction
  | EndAction
  | PlaceWorker Worker EdgeId Int
  | ReplaceWorker Worker Worker EdgeId Int
  | PickUp Worker EdgeId Int
  | EvHire Worker Int
  | Retire Worker Int
    deriving Show

instance ToJSON Event where
  toJSON ev =
    case ev of
      StartTurn p -> jsTagged "start-turn" [ "player" .= p ]
      EndTurn p   -> jsTagged "end-turn" [ "player" .= p ]
      StartAction -> jsTagged "start-action" []
      EndAction   -> jsTagged "end-action" []

      EvHire w n ->
        jsTagged "hire" [ "worker" .= w, "number" .= n ]

      Retire w n ->
        jsTagged "retire" [ "worker" .= w, "number" .= n ]

      PickUp w edgeId spot ->
        jsTagged "pick-up" [ "worker" .= w, "edge" .= edgeId, "spot" .= spot ]

      PlaceWorker w e s ->
        jsTagged "place-worker" [ "worker" .= w, "edge" .= e, "spot" .= s ]

      ReplaceWorker w1 w2 e s ->
        jsTagged "replace-worker" [ "workerOld" .= w1, "workerNew" .= w2
                                  , "edge" .= e, "spot" .= s ]


