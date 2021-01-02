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
  | ChEdge EdgeId Int WorkerType (Maybe Worker)
  | ChDone Text
    deriving (Eq,Ord,Show)


instance JS.FromJSON Choice where
  parseJSON = JS.withObject "choice" \o ->
    do tag <- o .: "tag"
       case tag :: Text of
         "prefer" -> ChSetPreference <$> (o .: "worker")
         "edge-empty" ->
           ChEdge <$> (o .: "edge") <*> (o .: "spot") <*> (o .: "shape")
                                    <*> pure Nothing
         _ -> fail "XXX: more choices"

instance JS.ToJSON Choice where
  toJSON ch =
    case ch of
      ChSetPreference wt  -> jsTagged "prefer"  [ "worker" .= wt ]
      ChActiveWorker wt   -> jsTagged "active"  [ "worker" .= wt ]
      ChPassiveWorker wt  -> jsTagged "passive" [ "worker" .= wt ]
      ChBonusToken bt     -> jsTagged "bonus"   [ "bonus"  .= bt ]
      ChEdge eid spot sh mbw ->
        case mbw of
          Nothing -> jsTagged "edge-empty" [ "edge" .= eid, "spot" .= spot
                                           , "shape" .= sh ]
          Just w  -> jsTagged "edge-full"  [ "edge" .= eid, "spot" .= spot
                                           , "shape" .= sh
                                           , "worker" .= w ]
      ChDone t -> jsTagged "done" [ "message" .= t ]

