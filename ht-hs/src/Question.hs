module Question where

import Data.Text(Text)
import Data.Aeson((.:))
import qualified Data.Aeson as JS
import Data.Aeson((.=))

import Common.Utils

import Basics
import Bonus



data Choice =
    ChSetPreference WorkerType
  | ChActiveWorker WorkerType
  | ChPassiveWorker WorkerType
  | ChBonusToken BonusToken
  | ChEdgeEmpty EdgeId Int WorkerType
  | ChEdgeFull  EdgeId Int (Maybe WorkerType) Worker
  | ChNodeEmpty NodeId WorkerType
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
              <*> (o .: "worker")

         "edge-full" ->
           ChEdgeFull
             <$> (o .: "edge")
             <*> (o .: "spot")
             <*> (o .: "shape")
             <*> (o .: "worker")

         "node-empty" ->
           ChNodeEmpty
            <$> (o .: "node")
            <*> (o .: "shape")

         "done" -> ChDone <$> (o .: "message")

         "active"  -> ChActiveWorker  <$> (o .: "worker")
         "passive" -> ChPassiveWorker <$> (o .: "worker")
         -- "bonus"   -> ChBonusToken    <$> (o .: "worker")
         _ -> fail "XXX: more choices"

instance JS.ToJSON Choice where
  toJSON ch =
    case ch of
      ChSetPreference wt  -> jsTagged "prefer"  [ "worker" .= wt ]
      ChActiveWorker wt   -> jsTagged "active"  [ "worker" .= wt ]
      ChPassiveWorker wt  -> jsTagged "passive" [ "worker" .= wt ]
      ChBonusToken bt     -> jsTagged "bonus"   [ "bonus"  .= bt ]
      ChEdgeEmpty eid w   -> jsTagged "edge-empty"
                                          [ "edge" .= eid, "worker" .= w ]

      ChEdgeFull eid spot sh w ->
        jsTagged "edge-full"  [ "edge" .= eid
                              , "spot" .= spot
                              , "shape" .= sh
                              , "worker" .= w
                              ]

      ChNodeEmpty nid spot sh ->
        jsTagged "node-empty" [ "node" .= nid, "spot" .= spot, "shape" .= sh ]

      ChDone t -> jsTagged "done" [ "message" .= t ]


