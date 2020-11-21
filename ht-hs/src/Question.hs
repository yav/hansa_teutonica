module Question where

import Basics
import Bonus

data Choice =
    ChActiveWorker WorkerType
  | ChPassiveWorker WorkerType
  | ChBonusToken BonusToken
  | ChEdge EdgeId RequireWorker (Maybe Worker)
  | ChNode NodeId NodeChoice
  | ChDone
    deriving (Show,Eq,Ord)

data NodeChoice =
    ChCity
  | ChAction Int      -- ^ One of the yellow actions in a city
  | ChOffice Int      -- ^ Index from the right
    deriving (Show,Eq,Ord)

data Question = Question
  { questionPlayer  :: PlayerColor
  , questoinAnswers :: [Choice]
  } deriving Show




