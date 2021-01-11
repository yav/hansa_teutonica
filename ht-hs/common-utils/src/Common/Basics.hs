module Common.Basics where

import Data.Text(Text)
import Data.Aeson (ToJSON(..),FromJSON,(.=))
import qualified Data.Aeson as JS

import Common.Utils


newtype PlayerId  = PlayerId Text
  deriving (Show,Read,Eq,Ord)

data WithPlayer a = PlayerId :-> a
  deriving (Eq,Ord,Show,Read)

instance Functor WithPlayer where
  fmap f (p :-> q) = p :-> f q

playerAnnot :: WithPlayer a -> PlayerId
playerAnnot (p :-> _) = p

--------------------------------------------------------------------------------
instance JSKey PlayerId where
  jsKey (PlayerId t) = t

instance ToJSON PlayerId where
  toJSON = jsEnum

instance FromJSON PlayerId where
  parseJSON = JS.withText "player id" \txt -> pure (PlayerId txt)

instance ToJSON a => ToJSON (WithPlayer a) where
  toJSON (p :-> a) = JS.object [ "player" .= p, "thing" .= a ]

