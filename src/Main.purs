module Main where

import Prelude
import RxJS.Observable (Observable, scan, startWith)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object (empty, singleton)
import Meiosis (run)
import Meiosis.Dom (ActionCreator, createActionCreator, createDomDriver)
import Snabbdom (VNodeData, VNodeEventObject, VNodeHookObjectProxy, VNodeProxy(..), toVNodeEventObject, toVNodeHookObjectProxy)
import SnabbdomHelpers (h, sel, props, on, children, text)

type Sinks = { dom :: Observable VNodeProxy }
type Sources = { dom :: Observable Action }

type State = Int

type Action = {
  name :: String,
  value :: Int
}

view :: ActionCreator -> State -> VNodeProxy
view a s =
  h # sel "div#app"
    # children
    [ h # sel "strong#msg"
        # children [text $ "Counter: " <> (show s)]
    , h # sel "button"
        # on (singleton "click" (a { name: "increase", value: 1}))
        # children [text "Increase"]
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

