module Question where

import Data.Text(Text)
import Data.Aeson((.:))
import qualified Data.Aeson as JS
import Data.Aeson((.=))

import Common.Utils

import Basics
import Bonus
import Stats



data Choice =
    ChSetPreference WorkerType
  | ChActiveWorker WorkerType
  | ChPassiveWorker WorkerType
  | ChBonusToken BonusToken
  | ChEdgeEmpty EdgeId Int WorkerType
  | ChEdgeFull  EdgeId Int (Maybe WorkerType) Worker
  | ChEdge      EdgeId
  | ChNodeEmpty NodeId WorkerType
  | ChNodeFull  NodeId Int
  | ChNodeUpgrade NodeId Stat
  | ChEndVPSpot Level
  | ChDone Text
    deriving (Eq,Ord,Show,Read)

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

         "edge" -> ChEdge <$> (o .: "edge")

         "node-empty"   -> ChNodeEmpty <$> (o .: "node") <*> (o .: "shape")
         "node-full"    -> ChNodeFull  <$> (o .: "node") <*> (o .: "spot")
         "node-upgrade" -> ChNodeUpgrade <$> (o .: "node") <*> (o .: "action")

         "end-vp" -> ChEndVPSpot <$> (o .: "level")

         "done" -> ChDone <$> (o .: "message")

         "active"  -> ChActiveWorker  <$> (o .: "worker")
         "passive" -> ChPassiveWorker <$> (o .: "worker")
         "bonus"   -> ChBonusToken    <$> (o .: "bonus")
         _ -> fail "Unknown choice"

instance JS.ToJSON Choice where
  toJSON ch =
    case ch of
      ChSetPreference wt  -> jsTagged "prefer"  [ "worker" .= wt ]
      ChActiveWorker wt   -> jsTagged "active"  [ "worker" .= wt ]
      ChPassiveWorker wt  -> jsTagged "passive" [ "worker" .= wt ]
      ChBonusToken bt     -> jsTagged "bonus"   [ "bonus"  .= bt ]

      ChEdge eid          -> jsTagged "edge" [ "edge" .= eid ]
      ChEdgeEmpty eid spot sh -> jsTagged "edge-empty"
                                          [ "edge"  .= eid
                                          , "spot"  .= spot
                                          , "shape" .= sh ]

      ChEdgeFull eid spot sh w ->
        jsTagged "edge-full"  [ "edge" .= eid
                              , "spot" .= spot
                              , "shape" .= sh
                              , "worker" .= w
                              ]

      ChNodeEmpty nid sh ->
        jsTagged "node-empty" [ "node" .= nid, "shape" .= sh ]

      ChNodeFull nid spot ->
        jsTagged "node-full" [ "node" .= nid, "spot" .= spot ]

      ChNodeUpgrade nid act ->
        jsTagged "node-upgrade" [ "node" .= nid, "action" .= act ]

      ChEndVPSpot lvl -> jsTagged "end-vp" [ "level" .= lvl ]

      ChDone t -> jsTagged "done" [ "message" .= t ]


