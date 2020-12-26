module Question where

import Data.Text(Text)
import Data.Aeson((.:))
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
  parseJSON = JS.withObject "choice" \o ->
    do tag <- o .: "tag"
       case tag :: Text of
         _ -> fail "XXX: more choices"

instance JS.ToJSON Choice where
  toJSON _ = JS.toJSON ("XXX: Choice" :: Text)

