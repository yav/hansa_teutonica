module Question where

import Data.Text(Text)
import Data.Aeson((.:))
import qualified Data.Aeson as JS
import Data.Aeson((.=))
import GHC.Generics

import Common.Utils

import Basics
import Bonus
import Stats



data Choice =
    ChSetPreference WorkerType
  | ChActiveWorker WorkerType
  | ChPassiveWorker WorkerType
  | ChUpgrade Stat
  | ChBonusToken BonusToken

  | ChEdgeEmpty EdgeId Int WorkerType
  | ChEdgeFull  EdgeId Int (Maybe WorkerType) Worker
  | ChEdge      EdgeId

  | ChNodeEmpty NodeId WorkerType
  | ChNodeAnnex NodeId WorkerType
  | ChNodeFull  NodeId Int
  | ChNodeUpgrade NodeId Stat
  | ChEndVPSpot Level

  | ChDone Text
    deriving (Eq,Ord,Show,Generic)

instance JS.FromJSON Choice
instance JS.ToJSON Choice

