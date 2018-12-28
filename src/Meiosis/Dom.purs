module Meiosis.Dom
  ( ActionCreator
  , createActionCreator
  , createDomDriver
  ) where

import Prelude

import Control.Comonad (extract)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object, empty)
import Meiosis (Driver, createSubjectDriver)
import RxJS.Observable (Observable, scan, subscribeNext)
import Snabbdom (VNodeData, VNodeEventObject, VNodeHookObjectProxy, VNodeProxy(..), patch, toVNode, toVNodeEventObject, toVNodeHookObjectProxy)
import Snabbdom (h) as Snabbdom
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

-- TODO: Add to snabbdom
type VNodeAttrsObject = Object String

type ActionCreator = forall a e. a -> (e -> Effect Unit)

foreign import createActionCreator :: forall a. Observable a -> ActionCreator

getElement :: String -> Effect (Maybe Element)
getElement selector = do
  doc <- liftEffect $ toDocument <$> (document =<< window)
  element <- getElementById selector $ toNonElementParentNode doc
  pure element

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
