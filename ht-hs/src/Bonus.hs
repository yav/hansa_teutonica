module Bonus where

import Data.Text(Text)
import qualified Data.Aeson as JS
import GHC.Generics

import Common.Utils(enumAll)

data BonusToken =
    BonusUpgrade
  | BonusSwap
  | BonusMove
  | BonusExtra
  | BonusAct3
  | BonusAct4
  deriving (Eq,Ord,Show,Generic,Enum,Bounded)

-- | Not counting the starting ones
tokenNumber :: BonusToken -> Int
tokenNumber token =
  case token of
    BonusUpgrade -> 2
    BonusAct3    -> 2
    BonusAct4    -> 2
    BonusSwap    -> 2
    BonusMove    -> 1
    BonusExtra   -> 3


startTokens :: [BonusToken]
startTokens = [ BonusExtra, BonusMove, BonusSwap ]

tokenList :: [BonusToken]
tokenList = [ tok | ty <- enumAll, tok <- replicate (tokenNumber ty) ty ]

data FixedBonus =
    BonusPlace2
  | BonusMove2
  | BonusGainPrivilege
  | BonusBuildInGreen
  | BonusReuse2
  deriving (Eq,Ord,Show)



--------------------------------------------------------------------------------
bonusAsKey :: BonusToken -> Text
bonusAsKey token =
  case token of
    BonusUpgrade -> "BonusUpgrade"
    BonusSwap    -> "BonusSwap"
    BonusMove    -> "BonusMove"
    BonusExtra   -> "BonusExtra"
    BonusAct3    -> "BonusAct3"
    BonusAct4    -> "BonusAct4"

instance JS.ToJSON    BonusToken
instance JS.ToJSONKey BonusToken
instance JS.FromJSON  BonusToken



