module Main where

import Board
import qualified Board.Britannia45 as B45
import qualified Board.Britannia3  as B3
import qualified Board.Hansa45 as H45
import qualified Board.Hansa3  as H3
import qualified Board.East as E
import Text.Show.Pretty(pPrint)

main :: IO ()
main =
  do writeFile "q_britannia_45.js" (exportLayout B45.board)
     writeFile "q_britannia_23.js" (exportLayout B3.board)
     writeFile "q_ht_45.js" (exportLayout H45.board)
     writeFile "q_ht_23.js" (exportLayout H3.board)
     writeFile "q_east.js" (exportLayout E.board)
