module Interact where

import qualified Data.Aeson as JS

import Game
import Basics



data SystemInMsg =
    Disconnected
  | Connected

data InMsg = System SystemInMsg | External ExternalInMsg

data OutMsg = PlaceWorkerOnEdge EdgeId Worker

data ExternalInMsg = InMsg

instance JS.ToJSON OutMsg where
  toJSON = undefined

instance JS.FromJSON ExternalInMsg where
  parseJSON = undefined

handleMessage ::
  (PlayerColor,InMsg) -> Interaction -> (Interaction, [(PlayerColor,OutMsg)])
handleMessage (player,msg) s = (s,[])


