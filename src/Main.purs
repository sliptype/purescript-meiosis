module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Data.Maybe (Maybe(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM (Element)
import Web.DOM.Document (Document, toNonElementParentNode, url)
import Web.DOM.Element (id)
import Web.DOM.NonElementParentNode (getElementById)

main :: Effect Unit
main = do
  doc <- liftEffect $ toDocument <$> (document =<< window)
  elementResult <- getElementById "app" $ toNonElementParentNode doc
  case elementResult of
    Just element -> log =<< id element
    Nothing -> log "Element not found"
