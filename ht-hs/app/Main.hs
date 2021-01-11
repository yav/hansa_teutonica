module Main(main) where

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Environment(getArgs)
import System.Random.TF(newTFGen)

import Common.Server

import Game(initialGame)
import Actions(nextAction)
import Basics
import Board.Index

main :: IO ()
main =
  do args <- getArgs
     case args of
       b : ps ->
         case Map.lookup (Text.pack b) boards of
           Just board ->
             do rng <- newTFGen
                let mkP = PlayerId . Text.pack
                    players = Set.fromList (map mkP ps)
                newServer GameInfo
                  { gPlayers = players
                  , gState = initialGame rng board players
                  , gInit = nextAction
                  }
           Nothing -> fail "unknown board"
       _ -> fail "Usage: board_name player1 player2 ..."



