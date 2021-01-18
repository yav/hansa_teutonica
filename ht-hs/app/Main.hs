module Main(main) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Environment(getArgs)
import System.Random.TF(newTFGen)

import Common.CallJS
import Common.Server

import Game(initialGame)
import Actions(nextAction)
import Basics
import Board.Index

import Game
import Event(Event,EventElement)
import Question(Choice)
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
                    str = $(jsHandlers [ ''EventElement,
                                         ''OutMsg,
                                         ''GameUpdate, ''Choice, ''Event])
                putStrLn str
                newServer (BS8.pack str) GameInfo
                  { gPlayers = players
                  , gState = initialGame rng board players
                  , gInit = do let w1 = Worker { owner = mkP (ps !! 0)
                                               , shape = Cube }
                               let w2 = Worker { owner = mkP (ps !! 1)
                                               , shape = Cube }
                               update (PlaceWorkerInOffice 0 w1)
                               update (PlaceWorkerInOffice 0 w2)
                               update (ChangeVP (owner w1) 19)
                               nextAction
                  }
           Nothing -> fail "unknown board"
       _ -> fail "Usage: board_name player1 player2 ..."



