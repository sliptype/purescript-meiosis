module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Map (empty)
import Snabbdom (VNodeProxy, h, patchInitialSelector, text, toVNodeEventObject, toVNodeHookObjectProxy)

createVNode :: String ->  VNodeProxy
createVNode message = h "strong#msg"
  { attrs : empty
  , on : toVNodeEventObject empty
  , hook : toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing}
  }
  [text message]

main :: Effect Unit
main = do
  patchInitialSelector "#app" $ createVNode "Hello world"
