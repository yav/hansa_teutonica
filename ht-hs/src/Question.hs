module Question where

import Basics
import Edge
import Bonus

data Choice =
    ChosseActiveWorker WorkerType
  | ChossePassiveWorker WorkerType
  | ChosseBonusToken BonusToken
  | OnEdge EdgeId EdgeSpotType (Maybe Worker)
  | InNode NodeId NodeChoice
  | ChooseDone
    deriving (Show,Eq,Ord)

data NodeChoice =
    ChooseCity
  | ChooseAction Int      -- ^ One of the yellow actions in a city
  | ChooseOffice Int      -- ^ Index from the right
    deriving (Show,Eq,Ord)

data Question = Question
  { questionPlayer  :: PlayerColor
  , questoinAnswers :: [Choice]
  } deriving Show




