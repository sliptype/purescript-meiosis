module Main where

import Prelude
import RxJS.Observable (Observable, scan, startWith)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object (empty, singleton)
import Meiosis (run)
import Meiosis.Dom (ActionCreator, createActionCreator, createDomDriver, emptyVNodeData)
import Snabbdom (VNodeData, VNodeEventObject, VNodeHookObjectProxy, VNodeProxy(..), h, text, toVNodeEventObject, toVNodeHookObjectProxy)

type Sinks = { dom :: Observable VNodeProxy }
type Sources = { dom :: Observable Action }

type State = Int

type Action = {
  name :: String,
  value :: Int
}

view :: ActionCreator -> State -> VNodeProxy
view a s =
  h "div#app" emptyVNodeData
    [ h "strong#msg" emptyVNodeData [text $ "Counter: " <> (show s)]
-- todo remove this repetition
    , h "button" { attrs: empty
                 , on: toVNodeEventObject $ singleton "click" (a { name: "increase"
                                            , value: 1
                                            })
                 , hook: toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing }
                 } [text "Increase"]
  ]

reducer :: Action -> State -> State
reducer _a s = s + 1

noOp :: Action
noOp =
  { name: "noop"
  , value: 0
}


app :: Sources -> Sinks
app { dom: a } = let act = createActionCreator a in
  { dom: a
      # startWith noOp
      # scan reducer 100
      # map (view act)
  }

main :: Effect Unit
main = do
  d <- createDomDriver "app"
  run app (singleton "dom" d)

