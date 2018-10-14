module Main where

import Prelude
import Effect (Effect)
import Control.Comonad (extract)
import Data.Maybe (Maybe(..))
import Data.Map (empty)
import Snabbdom (VNodeProxy, VNodeData, h, patchInitialSelector, patch, text, toVNodeEventObject, toVNodeHookObjectProxy)
import RxJS.Observable

type State = Int

emptyVNodeData :: VNodeData
emptyVNodeData =
  { attrs : empty
  , on : toVNodeEventObject empty
  , hook : toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing }
  }

initialState :: State
initialState = 10

update :: Observable State
update = interval 1000 # map (\s -> s + 1) # startWith initialState

view :: State -> VNodeProxy
view s = h "div" emptyVNodeData
  [ h "strong#msg" emptyVNodeData [text $ "Counter: " <> (show s)]
  , h "button" emptyVNodeData [text "Increase"]
  ]

main :: Effect Unit
main = do
  vnode <- patchInitialSelector "#app" $ view initialState
  update # subscribe vnode

subscribe :: VNodeProxy -> Observable State -> Effect Unit
subscribe vnode obs = do
  sub <- extract (obs # map view # subscribeNext (patch vnode))
  pure unit
