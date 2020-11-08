module Utils where

enumAll :: (Bounded a,Enum a) => [a]
enumAll = [ minBound .. maxBound ]

times :: Int -> (a -> a) -> (a -> a)
times n f = (!! n) . iterate f


