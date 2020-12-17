module Question where

import qualified Data.Aeson as JS

import Basics
import Bonus
import Node

data Choice =
    ChActiveWorker WorkerType
  | ChPassiveWorker WorkerType
  | ChBonusToken BonusToken
  | ChEdge EdgeId RequireWorker (Maybe Worker)
  | ChAction NodeAction
  | ChDone
    deriving (Show)


{-
instance JS.FromJSON Choice where
  parseJSON = JS.withObject "player choice" \fields ->
    do choice <- fields .: "choose"
       case choice of
instance JS.FromJSON NodeChoice where
-}


