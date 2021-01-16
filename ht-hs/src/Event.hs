module Event where

import Data.Aeson(ToJSON)
import GHC.Generics

import Basics
import Stats
import Bonus

-- User readable events describing the game flow
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
  | BuildAnnnex NodeId Worker BonusToken
  | Upgraded PlayerId Stat
  | Invested NodeId Int Worker
  | PlacedBonus EdgeId BonusToken
  | UsedBonus BonusToken
  | SwappedWorkers NodeId Int BonusToken
    deriving (Show,Generic)

instance ToJSON Event

