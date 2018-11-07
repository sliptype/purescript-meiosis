module Meiosis
       ( Driver
       , ActionCreator
       , run
       , createDomDriver
       , createActionCreator
       , emptyVNodeData
       ) where

import Prelude
import RxJS.Observable (Observable, scan, subscribeNext)

import Control.Comonad (extract)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object, empty)
import Snabbdom (VNodeData, VNodeEventObject, VNodeHookObjectProxy, VNodeProxy, patch, toVNode, toVNodeEventObject, toVNodeHookObjectProxy)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

-- TODO: Add to snabbdom
type VNodeAttrsObject = Object String

type Driver a b = Observable a -> Observable b

foreign import run :: forall a b c d. (a -> b) -> Object (Driver c d) -> Effect Unit

-- domDriver
type ActionCreator = forall a e. a -> (e -> Effect Unit)

foreign import createActionCreator :: forall a. Observable a -> ActionCreator

getElement :: String -> Effect (Maybe Element)
getElement selector = do
  doc <- liftEffect $ toDocument <$> (document =<< window)
  element <- getElementById selector $ toNonElementParentNode doc
  pure element

foreign import createSubjectDriver :: forall a b. (Observable a -> Effect Unit) -> Driver a b

domDriver :: Maybe Element -> Observable VNodeProxy -> Effect Unit
domDriver e v = do
  case e of
    Just (element) -> do
      _ <- extract $ v
        # scan (\a b -> unsafePerformEffect $ patch b a) (toVNode element)
        # subscribeNext pure
      pure unit
    Nothing -> do
      log "Element could not be found by id"

createDomDriver :: forall a. String -> Effect (Driver VNodeProxy a)
createDomDriver id = do
  element <- getElement "app"
  pure $ createSubjectDriver $ domDriver element

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
