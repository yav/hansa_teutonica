module Game where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

import qualified Data.Aeson as JS
import Data.Aeson ((.=))

import Basics
import Bonus
import Player
import Board
import Question

import qualified Board.Britannia45 as B45

data Game = Game
  { gamePlayers   :: Map PlayerColor Player
  , gameTurnOrder :: [PlayerColor]
  , gameBoard     :: Board
  , gameStatus    :: GameStatus
  } deriving Show


data GameStatus =
    InProgress Turn
  | Finished
    deriving Show

data Turn = Turn
  { turnCurrentPlayer :: PlayerColor
  , turnActionsDone   :: Int
  , turnActionLimit   :: Int
  , turnUsedGateways  :: Set ProvinceId
  , turnPlaceBonus    :: [BonusToken]
  } deriving Show

getPlayer :: Game -> PlayerColor -> Player
getPlayer Game { gamePlayers } p = gamePlayers Map.! p


-- XXX: just testing
initialGameState :: Game
initialGameState = Game
  { gamePlayers = Map.fromList [ (Yellow, initialPlayer 0)
                               , (Red,    initialPlayer 1)
                               ]
  , gameTurnOrder = [ Yellow, Red ]
  , gameBoard  = B45.board
  , gameStatus = InProgress
    Turn
      { turnCurrentPlayer = Yellow
      , turnActionsDone   = 0
      , turnActionLimit   = 5
      , turnUsedGateways  = Set.empty
      , turnPlaceBonus    = []
      }
  }


data SystemInMsg =
    Disconnected
  | Connected

data InMsg = System SystemInMsg | External ExternalInMsg

data OutMsg = PlaceWorkerOnEdge EdgeId Worker

data ExternalInMsg = InMsg


--------------------------------------------------------------------------------
handleMessage ::
  (PlayerColor,InMsg) -> Game -> (Game, [(PlayerColor,OutMsg)])
handleMessage (player,msg) s = (s,[])



--------------------------------------------------------------------------------
instance JS.ToJSON OutMsg where
  toJSON = undefined

instance JS.FromJSON ExternalInMsg where
  parseJSON = undefined


instance JS.ToJSON Game where
  toJSON g = JS.object
    [ "players" .=
        JS.object [ playerColorToKey pId .= p
                  | (pId,p) <- Map.toList (gamePlayers g)
                  ]
    , "turnOrder" .= gameTurnOrder g
    , "board"     .= gameBoard g
    , "status"    .= gameStatus g
    ]

instance JS.ToJSON GameStatus where
  toJSON status =
    case status of
      InProgress t -> JS.toJSON t
      Finished     -> JS.Null


instance JS.ToJSON Turn where
  toJSON t = JS.object
    [ "player"   .= turnCurrentPlayer t
    , "actDone"  .= turnActionsDone t
    , "actLimit" .= turnActionLimit t
    , "bonuses"  .= length (turnPlaceBonus t)
    ]
