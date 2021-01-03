module Question where

import Data.Text(Text)
import Data.Aeson((.:))
import qualified Data.Aeson as JS
import Data.Aeson((.=))

import Utils
import Basics
import Bonus
import Node

import Debug.Trace


data Choice =
    ChSetPreference WorkerType
  | ChActiveWorker WorkerType
  | ChPassiveWorker WorkerType
  | ChBonusToken BonusToken
  | ChEdgeEmpty EdgeId Int WorkerType
  | ChEdgeFull  EdgeId Int (Maybe WorkerType) Worker
  | ChDone Text
    deriving (Eq,Ord,Show)

instance JS.FromJSON Choice where
  parseJSON = JS.withObject "choice" \o ->
    do tag <- o .: "tag"
       case tag :: Text of

         "prefer" -> ChSetPreference <$> (o .: "worker")

         "edge-empty" ->
           ChEdgeEmpty
              <$> (o .: "edge")
              <*> (o .: "spot")
              <*> (o .: "shape")

         "edge-full" ->
           ChEdgeFull
             <$> (o .: "edge")
             <*> (o .: "spot")
             <*> (o .: "shape")
             <*> (o .: "worker")

         "done" -> ChDone <$> (o .: "message")
         _ -> fail "XXX: more choices"

instance JS.ToJSON Choice where
  toJSON ch =
    case ch of
      ChSetPreference wt  -> jsTagged "prefer"  [ "worker" .= wt ]
      ChActiveWorker wt   -> jsTagged "active"  [ "worker" .= wt ]
      ChPassiveWorker wt  -> jsTagged "passive" [ "worker" .= wt ]
      ChBonusToken bt     -> jsTagged "bonus"   [ "bonus"  .= bt ]
      ChEdgeEmpty eid spot sh ->
        jsTagged "edge-empty" [ "edge" .= eid, "spot" .= spot, "shape" .= sh ]

      ChEdgeFull eid spot sh w ->
        jsTagged "edge-full"  [ "edge" .= eid
                              , "spot" .= spot
                              , "shape" .= sh
                              , "worker" .= w
                              ]

      ChDone t -> jsTagged "done" [ "message" .= t ]


