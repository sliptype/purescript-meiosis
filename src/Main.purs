module Main where

import Prelude
import RxJS.Observable
import RxJS.Subscriber

import Control.Comonad (extract)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (Object, empty, insert, singleton)
import Snabbdom (VNodeData, VNodeEventObject, VNodeHookObject, VNodeProxy(..), h, patch, patchInitialSelector, text, toVNode, toVNodeEventObject, toVNodeHookObjectProxy)
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

foreign import createSubjectDriver :: forall a b c. Driver a b -> Driver a c

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

subscribe :: VNodeProxy -> Observable VNodeProxy -> Effect Unit
subscribe vnode obs = do
  sub <- extract (obs # subscribeNext (patch vnode))
  pure unit

domDriver :: Effect (Driver VNodeProxy Action)
domDriver v = do
  element <- getElement "#app"
  case element of
    Just element -> pure $ createSubjectDriver $ subscribe (toVNode element) v
    Nothing -> log "Element not found"

emptyVNodeData :: VNodeData
emptyVNodeData =
  { attrs : empty
  , on : toVNodeEventObject empty
  , hook : toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing }
  }

createVNodeData :: VNodeAttrsObject -> VNodeEventObject -> VNodeHookObject
createVNodeData attrs events hooks =
  { attrs : attrs
  , on : events
  , hook : hooks
  }

-- userland

type State = Int

view :: ActionCreator -> State -> VNodeProxy
view a s =
  h "div" emptyVNodeData
    [ h "strong#msg" emptyVNodeData [text $ "Counter: " <> (show s)]
-- todo remove this repetition
    , h "button" { attrs: empty
                 , on: toVNodeEventObject $ singleton "click" (a { name: "increase"
                                            , value: 1
                                            })
                 , hook: toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing }
                 } [text "Increase"]
  ]

reducer :: State -> Action -> State
reducer s _a = s + 1

app :: Sources -> Sinks
app { dom: a } = let act = createActionCreator a in
  { dom: a
      # startWith 0
      # scan reducer
      # map (view a)
  }

main :: Effect Unit
main = run app (singleton "dom" domDriver)

