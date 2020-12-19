module Question where

import qualified Data.Aeson as JS

import Basics
import Bonus
import Node



data Choice =
    ChActiveWorker WorkerType
  | ChPassiveWorker WorkerType
  | ChBonusToken BonusToken
  | ChEdge EdgeId Int (Maybe Worker)
  | ChAction NodeAction
  | ChDone
    deriving (Eq,Ord,Show)


instance JS.FromJSON Choice where
  parseJSON = error "XXX"


