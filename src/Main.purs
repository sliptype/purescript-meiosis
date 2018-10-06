module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Map (empty, singleton)
import Snabbdom (VNodeProxy, VNodeData, h, patchInitialSelector, text, toVNodeEventObject, toVNodeHookObjectProxy)

type State = Int

emptyVNodeData :: VNodeData
emptyVNodeData =
  { attrs : empty
  , on : toVNodeEventObject empty
  , hook : toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing}
  }

initialState :: State
initialState = 10

view :: State -> VNodeProxy
view s = h "div" emptyVNodeData
  [ h "strong#msg" emptyVNodeData [text $ "Counter: " <> (show s)]
  , h "button" emptyVNodeData [text "Increase"]
  ]

main :: Effect Unit
main = do
  patchInitialSelector "#app" $ view initialState
