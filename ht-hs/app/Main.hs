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

import Game
import Common.Interact

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
                  , gInit = do let w1 = Worker { workerOwner = mkP (ps !! 0)
                                               , workerType = Cube }
                               let w2 = Worker { workerOwner = mkP (ps !! 1)
                                               , workerType = Cube }
                               update (PlaceWorkerInOffice 0 w1)
                               update (PlaceWorkerInOffice 0 w2)
                               nextAction
                  }
           Nothing -> fail "unknown board"
       _ -> fail "Usage: board_name player1 player2 ..."



