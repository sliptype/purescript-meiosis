module Main where

import Prelude
import RxJS.Observable
import RxJS.Subscriber

import Control.Comonad (extract)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object, empty, insert, singleton)
import Snabbdom (VNodeData, VNodeEventObject, VNodeHookObjectProxy, VNodeProxy(..), h, patch, patchInitialSelector, text, toVNode, toVNodeEventObject, toVNodeHookObjectProxy)
import Web.DOM.Document (Document, toNonElementParentNode, url)
import Web.DOM.Element (Element, id)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

-- TODO: Add to snabbdom
type VNodeAttrsObject = Object String

type Sinks = { dom :: Observable VNodeProxy }
type Sources = { dom :: Observable Action }
type Driver a b = Observable a -> Observable b

foreign import run :: forall a b. (Sources -> Sinks) -> Object (Driver a b) -> Effect Unit

-- domDriver
type ActionCreator = forall e. Action -> (e -> Effect Unit)

foreign import createActionCreator :: Observable Action -> ActionCreator

-- TODO: dynamic action types
type Action = {
  name :: String,
  value :: Int
}

getElement :: String -> Effect (Maybe Element)
getElement selector = do
  doc <- liftEffect $ toDocument <$> (document =<< window)
  element <- getElementById selector $ toNonElementParentNode doc
  pure element

foreign import createSubjectDriver :: forall a b. (Observable a -> Effect Unit) -> Driver a b

domDriver :: Element -> Observable VNodeProxy -> Effect Unit
domDriver e v = do
  _ <- extract $ v
    # scan (\a b -> unsafePerformEffect $ patch b a) (toVNode e)
    # subscribeNext pure
  pure unit

emptyVNodeData :: VNodeData
emptyVNodeData =
  { attrs : empty
  , on : toVNodeEventObject empty
  , hook : toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing }
  }

createVNodeData :: VNodeAttrsObject -> VNodeEventObject -> VNodeHookObjectProxy -> VNodeData
createVNodeData attrs events hooks =
  { attrs : attrs
  , on : events
  , hook : hooks
  }

-- userland

type State = Int

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
  element <- getElement "app"
  case element of
    Just element -> run app (singleton "dom" $ createSubjectDriver (domDriver element))
    Nothing -> log "Element not found"

