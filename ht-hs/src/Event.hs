module Event where

import Data.Aeson(ToJSON,(.=))
import qualified Data.Aeson as JS

import Common.Utils

import Basics
import Stats

-- User readable events desribing the game flow
data Event =
    StartTurn PlayerId
  | EndTurn PlayerId
  | StartAction
  | EndAction
  | PlaceWorker Worker EdgeId Int
  | MoveWorkerTo EdgeId Int Worker
  | ReplaceWorker Worker Worker EdgeId Int
  | PickUp Worker EdgeId Int
  | EvHire Worker Int
  | Retire Worker Int
  | GainVP PlayerId Int
  | CompleteRoute EdgeId
  | BuildOffice NodeId Worker
  | Upgraded PlayerId Stat
  | Invested NodeId Int Worker
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

      Upgraded p a ->
        jsTagged "upgrade" [ "player" .= p, "action" .= a ]

      Invested nodeId pts w ->
        jsTagged "invested" [ "node" .= nodeId, "points" .= pts, "worker" .= w ]

      PickUp w edgeId spot ->
        jsTagged "pick-up" [ "worker" .= w, "edge" .= edgeId, "spot" .= spot ]

      PlaceWorker w e s ->
        jsTagged "place-worker" [ "worker" .= w, "edge" .= e, "spot" .= s ]

      MoveWorkerTo e s w ->
        jsTagged "move-worker" [ "worker" .= w, "edge" .= e, "spot" .= s ]

      ReplaceWorker w1 w2 e s ->
        jsTagged "replace-worker" [ "workerOld" .= w1, "workerNew" .= w2
                                  , "edge" .= e, "spot" .= s ]

      GainVP p n -> jsTagged "vp" [ "player" .= p, "vp" .= n ]
      CompleteRoute e -> jsTagged "complete-route" [ "edge" .= e ]
      BuildOffice n w -> jsTagged "build-office" [ "node" .= n, "worker" .= w ]


