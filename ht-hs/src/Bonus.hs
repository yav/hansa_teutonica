module Bonus where

data BonusToken =
    BonusUpgrade
  | BonusSwap
  | BonusMove
  | BonusExtra
  | BonusAct4
  | BonusAct3
  deriving (Eq,Ord,Show)


tokenNumber :: BonusToken -> Int
tokenNumber token =
  case token of
    BonusUpgrade -> 3
    BonusSwap    -> 2
    BonusMove    -> 2
    BonusExtra   -> 5
    BonusAct4    -> 2
    BonusAct3    -> 2


data FixedBonus =
    BonusPlace2
  | BonusMove2
  | BonusGainPrivilege
  | BonusBuildInGreen
  | BonusReuse2
  deriving (Eq,Ord,Show)


