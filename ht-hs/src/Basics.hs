module Basics
  ( module Basics
  , module Common.Basics
  ) where

import Data.Aeson ((.=),(.:))
import qualified Data.Aeson as JS
import Common.Utils

import Common.Basics

type NodeId       = Int      -- ^ Identifies a city
type EdgeId       = Int      -- ^ Identifies a route
type ProvinceId   = Int      -- ^ Identifies a route (Brittania expansion)

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

instance JSKey WorkerType where
  jsKey wt =
    case wt of
      Disc -> "disc"
      Cube -> "cube"

instance JS.ToJSON WorkerType where
  toJSON = jsEnum

instance JS.ToJSON Worker where
  toJSON worker =
    JS.object [ "owner" .= workerOwner worker
              , "shape" .= workerType worker
              ]

instance JS.FromJSON Worker where
  parseJSON = JS.withObject "worker" \o ->
    do owner <- o .: "owner"
       shape <- o .: "shape"
       pure Worker { workerOwner = owner, workerType = shape }


instance JS.FromJSON WorkerType where
  parseJSON = jsParseEnum "worker type"


