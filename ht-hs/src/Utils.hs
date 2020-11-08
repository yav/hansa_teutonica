module Utils where

enumAll :: (Bounded a,Enum a) => [a]
enumAll = [ minBound .. maxBound ]



