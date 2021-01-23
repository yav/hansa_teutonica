module Main(main) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import System.Environment(getArgs)
import System.Random.TF(mkSeedUnix,seedTFGen)
import Data.Word(Word64)

import Common.CallJS
import Common.Server

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
       ["load",file] ->
          do txt <- readFile file
             case reads txt of
               [(s,"")] -> begin s
               _ -> fail "Failed to load save"
       _ -> do seed <- mkSeedUnix
               let moves = []
               begin Save { .. }

data Save = Save
  { seed  :: (Word64, Word64, Word64, Word64)
  , args  :: [String]
  , moves :: [WithPlayer Choice]
  } deriving (Read,Show)

begin :: Save -> IO ()
begin Save { .. } =
  case args of
    b : ps ->
      case Map.lookup (Text.pack b) boards of
        Just board ->
          do let rng = seedTFGen seed
                 mkP = PlayerId . Text.pack
                 players = Set.fromList (map mkP ps)
                 str = $(jsHandlers [ ''EventElement,
                                      ''OutMsg,
                                      ''GameUpdate, ''Choice, ''Event])
             newServer (BS8.pack str)
               $ startGame GameInfo
                             { gPlayers = players
                             , gState = initialGame rng board players
                             , gInit = nextAction
                             , gSave = \moves -> show Save { .. }
                             } moves
        Nothing -> fail $ unlines $ "unknown board"
                                  : map Text.unpack (Map.keys boards)
  
    _ -> fail "usage: board_name player1_name player2_name ..."
