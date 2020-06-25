{-# Language ViewPatterns #-}
module Main where

import Text.Read(readMaybe)
import System.Environment(getArgs)

main =
  do fs <- getArgs
     case fs of
        (readMaybe -> Just n) : more -> count 0 n more
        _ -> putStrLn "Need an offset and some file."

count base off fs =
  case fs of
    [] -> putStrLn "Not found"
    f : more ->
      do len <- length.lines <$> readFile f
         if off <= len
           then putStrLn ("vim +" ++ show off ++ " " ++ show f)
           else count (base + len) (off - len) more
