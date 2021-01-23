module Actions.FixedBonus where

import Common.Basics
import Common.Interact

import Basics
import Bonus

doFixedBonus :: PlayerId -> EdgeId -> FixedBonus -> Interact ()
doFixedBonus playerId edgeId bonus =
  case bonus of
    BonusPlace2         -> pure ()
    BonusMove2          -> pure ()
    BonusGainPrivilege  -> pure ()
    BonusBuildInGreen   -> pure ()
    BonusReuse2         -> pure ()


{-
doPlaceInProvince :: PlayerId -> Int -> Int -> Interact ()
doPlaceInProvince playerId placing limit
  | placing > limit = pure ()
  | otherwise
    do player <- view (getField (gamePlayer playerId))
-}
