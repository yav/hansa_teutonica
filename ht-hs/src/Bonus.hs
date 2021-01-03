module Bonus where

import Data.Text(Text)
import qualified Data.Aeson as JS

import Common.Utils(enumAll)

data BonusToken =
    BonusUpgrade
  | BonusSwap
  | BonusMove
  | BonusExtra
  | BonusAct4
  | BonusAct3
  deriving (Eq,Ord,Show,Enum,Bounded)

tokenNumber :: BonusToken -> Int
tokenNumber token =
  case token of
    BonusUpgrade -> 3
    BonusSwap    -> 2
    BonusMove    -> 2
    BonusExtra   -> 5
    BonusAct4    -> 2
    BonusAct3    -> 2

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
    BonusUpgrade -> "upgrade"
    BonusSwap    -> "swap"
    BonusMove    -> "move"
    BonusExtra   -> "extra"
    BonusAct4    -> "act_4"
    BonusAct3    -> "act_3"

instance JS.ToJSON BonusToken where
  toJSON = JS.toJSON . bonusAsKey


