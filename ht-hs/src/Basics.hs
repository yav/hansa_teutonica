module Basics where

import Data.Text(Text)
import Data.Aeson ((.=))
import qualified Data.Aeson as JS

type NodeId       = Int      -- ^ Identifies a city
type EdgeId       = Int      -- ^ Identifies a route
type ProvinceId   = Int      -- ^ Identifies a route (Brittania expansion)


newtype PlayerId  = PlayerId Text
  deriving (Show,Eq,Ord)

data WithPlayer a = PlayerId :-> a
  deriving (Eq,Ord,Show)

instance Functor WithPlayer where
  fmap f (p :-> q) = p :-> f q


data WorkerType   = Cube | Disc
  deriving (Eq,Ord,Show,Bounded,Enum)

replacementCost :: WorkerType -> Int
replacementCost wt =
  case wt of
    Cube -> 1
    Disc -> 2

otherType :: WorkerType -> WorkerType
otherType ty =
  case ty of
    Cube -> Disc
    Disc -> Cube



data RequireWorker = AnyWorker | Require WorkerType
  deriving (Eq,Ord,Show)

accepts :: RequireWorker -> WorkerType -> Bool
accepts requirement workerType =
  case requirement of
    AnyWorker     -> True
    Require shape -> workerType == shape


data Worker = Worker
  { workerOwner :: PlayerId
  , workerType  :: WorkerType
  } deriving (Show,Eq,Ord)




--------------------------------------------------------------------------------
playerIdToKey :: PlayerId -> Text
playerIdToKey (PlayerId t) = t

instance JS.ToJSON PlayerId where
  toJSON = JS.toJSON . playerIdToKey

workerTypeToKey :: WorkerType -> Text
workerTypeToKey workerType =
  case workerType of
    Disc -> "disc"
    Cube -> "cube"

instance JS.ToJSON WorkerType where
  toJSON = JS.toJSON . workerTypeToKey

instance JS.ToJSON Worker where
  toJSON worker =
    JS.object [ "owner" .= workerOwner worker
              , "shape" .= workerType worker
              ]



instance JS.FromJSON WorkerType where
  parseJSON = JS.withText "worker type" \txt ->
    case txt of
      "cube" -> pure Cube
      "disc" -> pure Disc
      _      -> fail "Malformed worker type"

instance JS.FromJSON PlayerId where
  parseJSON = JS.withText "player id" \txt -> pure (PlayerId txt)

instance JS.ToJSON a => JS.ToJSON (WithPlayer a) where
  toJSON (p :-> a) = JS.object [ "player" .= p, "thing" .= a ]
