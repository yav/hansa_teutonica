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
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

-- | Not counting the starting ones
tokenNumber :: BonusToken -> Int
tokenNumber token =
  case token of
    BonusUpgrade -> 2
    BonusAct4    -> 2
    BonusAct3    -> 2
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
  deriving (Eq,Ord,Show,Read)



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

instance JS.FromJSON BonusToken where
  parseJSON = JS.withText "bonus token" \t ->
    case lookup t [ (bonusAsKey b,b) | b <- enumAll ] of
      Just a    -> pure a
      Nothing   -> fail "Invalid bonus token"



