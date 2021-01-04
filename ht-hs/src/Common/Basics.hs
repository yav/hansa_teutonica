module Common.Basics where

import Data.Text(Text)
import Data.Aeson (ToJSON,FromJSON,(.=))
import qualified Data.Aeson as JS



class ( ToJSON   app
      , ToJSON   (Update app)
      , ToJSON   (Input app)
      , FromJSON (Input app)
      , Ord      (Input app)
      ) => App app where
  type Update app
  type Input  app
  doUpdate :: Update app -> app -> app



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

