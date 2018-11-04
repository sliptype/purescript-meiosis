module Main where

import Prelude

import Control.Comonad (extract)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (Object, empty, insert, singleton)

import Web.DOM.Document (Document, toNonElementParentNode, url)
import Web.DOM.Element (Element, id)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

import RxJS.Observable
import RxJS.Subscriber
import Snabbdom
  ( VNodeData
  , VNodeEventObject
  , VNodeHookObjectProxy
  , VNodeProxy(..)
  , h
  , patch
  , patchInitialSelector
  , text
  , toVNode
  , toVNodeEventObject
  , toVNodeHookObjectProxy
  )

-- TODO: Add to snabbdom
type VNodeAttrsObject = Object String

type Sinks = { dom :: Observable VNodeProxy }
type Sources = { dom :: Observable Action }
type Driver a b = Observable a -> Observable b

foreign import run :: forall a b. (Sources -> Sinks) -> Object (Driver a b) -> Effect Unit

-- domDriver
foreign import createDomDriver :: String -> Driver VNodeProxy Action

type ActionCreator = forall e. Action -> (e -> Effect Unit)

foreign import createActionCreator :: Observable Action -> ActionCreator

-- TODO: dynamic action types
type Action = {
  name :: String,
  value :: Int
}

-- TODO: move into purescript
-- How to run Observable Effect ?

-- foreign import createSubjectDriver :: forall a b c. Driver a b -> Driver a c

-- getElement :: String -> Effect (Maybe Element)
-- getElement selector = do
--   doc <- liftEffect $ toDocument <$> (document =<< window)
--   element <- getElementById selector $ toNonElementParentNode doc
--   pure element

-- subscribe :: VNodeProxy -> Observable VNodeProxy -> Effect Unit
-- subscribe vnode obs = do
--   sub <- extract (obs # subscribeNext (patch vnode))
--   pure unit

-- domDriver :: Element -> Driver VNodeProxy (Effect VNodeProxy)
-- domDriver e v =
--   v
--   # scanM patch (toVNode e)

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
      # scan reducer 0
      # map (view act)
  }

main :: Effect Unit
main = run app (singleton "dom" (createDomDriver "#app"))

