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

state :: Observable State
state = interval 1000 # map (\s -> initialState + s)

view :: State -> VNodeProxy
view s = h "div" emptyVNodeData
  [ h "strong#msg" emptyVNodeData [text $ "Counter: " <> (show s)]
  , h "button" emptyVNodeData [text "Increase"]
  ]

main :: Effect Unit
main = do
  -- Render the initial state
  vnode <- patchInitialSelector "#app" $ view initialState
  -- Map state to view and attach a handler to re-render
  state # map view # subscribe vnode

subscribe :: VNodeProxy -> Observable VNodeProxy -> Effect Unit
subscribe vnode obs = do
  sub <- extract (obs # subscribeNext (patch vnode))
  pure unit
