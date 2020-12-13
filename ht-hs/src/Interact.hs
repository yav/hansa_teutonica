module Interact where

import qualified Data.Aeson as JS

import Game
import Basics



data SystemInMsg =
    Disconnected
  | Connected

data InMsg = System SystemInMsg | External ExternalInMsg

data OutMsg = OutMsg

data ExternalInMsg = InMsg

instance JS.ToJSON OutMsg where
  toJSON OutMsg = JS.object []

instance JS.FromJSON ExternalInMsg where
  parseJSON = undefined

handleMessage ::
  (PlayerColor,InMsg) -> Interaction -> (Interaction, [(PlayerColor,OutMsg)])
handleMessage (player,msg) s = (s,[])


