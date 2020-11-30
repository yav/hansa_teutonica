module Main where

import Board
import Board.Britannia45
import Text.Show.Pretty(pPrint)

main :: IO ()
main = putStrLn (exportLayout board)
