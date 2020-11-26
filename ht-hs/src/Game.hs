module Game where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)

import Basics
import Bonus
import Player
import Board
import Question

data Turn = Turn
  { turnCurrentPlayer :: PlayerColor
  , turnActionsDone   :: Int
  , turnActionLimit   :: Int
  , turnUsedGateways  :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  , turnGame          :: Game
  } deriving Show

data Game = Game
  { gamePlayers   :: Map PlayerColor Player
  , gameTurnOrder :: [PlayerColor]
  , gameBoard     :: Board
  } deriving Show

getPlayer :: Game -> PlayerColor -> Player
getPlayer Game { gamePlayers } p = gamePlayers Map.! p


data TargetedQuestion =
  Ask { askPlayer :: PlayerColor, askQuestion ::  Question }

data Question = Question
  { qAnswers  :: [Choice]        -- ^ What are their choices
  , qContinue :: Choice -> Either Turn TargetedQuestion
    -- ^ What to do with the answer
  }

instance Semigroup Question where
  q1 <> q2 =
    Question { qAnswers = qAnswers q1 ++ qAnswers q2
             , qContinue = \c -> if c `elem` qAnswers q1
                                    then qContinue q1 c
                                    else qContinue q2 c
             }




data PlayerAnswer = PlayerAnswer { paPlayer :: PlayerColor, paAnswer :: Choice }
  deriving Show

data Interaction = Interaction
  { iInitialTurn :: Turn -- ^ The turn at the start of the interaction
  , iLog         :: [PlayerAnswer]
    -- ^ Choices since the start of the iteratoin
  , iAsk  :: TargetedQuestion  -- ^ Waiting on this information
  }

step :: PlayerAnswer -> Interaction -> Either Interaction ([PlayerAnswer],Turn)
step answer i
  | paPlayer answer /= askPlayer question = Left i
  | otherwise =
    case qContinue (askQuestion question) (paAnswer answer) of
      Left turn -> Right (newLog, turn)
      Right q   -> Left i { iLog = newLog, iAsk = q }
  where
  newLog         = answer : iLog i
  question       = iAsk i



