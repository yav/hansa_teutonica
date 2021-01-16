module Common.Basics where

import Data.Text(Text)
import GHC.Generics
import Data.Aeson (ToJSONKey, ToJSON(..),FromJSON,(.=))
import qualified Data.Aeson as JS

import Common.Utils


newtype PlayerId  = PlayerId Text
  deriving (Show,Generic,Eq,Ord)

data WithPlayer a = PlayerId :-> a
  deriving (Eq,Ord,Show)

instance Functor WithPlayer where
  fmap f (p :-> q) = p :-> f q

playerAnnot :: WithPlayer a -> PlayerId
playerAnnot (p :-> _) = p

--------------------------------------------------------------------------------

instance ToJSONKey PlayerId where toJSONKey = jsDeriveKey
instance ToJSON PlayerId
instance FromJSON PlayerId

instance JSKey PlayerId where
  jsKey (PlayerId t) = t

instance ToJSON a => ToJSON (WithPlayer a) where
  toJSON (p :-> a) = JS.object [ "player" .= p, "thing" .= a ]

