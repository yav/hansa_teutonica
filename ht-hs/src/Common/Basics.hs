module Common.Basics where

import Data.Text(Text)
import Data.Aeson ((.=))
import qualified Data.Aeson as JS

newtype PlayerId  = PlayerId Text
  deriving (Show,Eq,Ord)

data WithPlayer a = PlayerId :-> a
  deriving (Eq,Ord,Show)

instance Functor WithPlayer where
  fmap f (p :-> q) = p :-> f q


--------------------------------------------------------------------------------
playerIdToKey :: PlayerId -> Text
playerIdToKey (PlayerId t) = t

instance JS.ToJSON PlayerId where
  toJSON = JS.toJSON . playerIdToKey

instance JS.FromJSON PlayerId where
  parseJSON = JS.withText "player id" \txt -> pure (PlayerId txt)

instance JS.ToJSON a => JS.ToJSON (WithPlayer a) where
  toJSON (p :-> a) = JS.object [ "player" .= p, "thing" .= a ]

