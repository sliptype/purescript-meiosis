module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.DOM (Element)
import Web.DOM.Document (url)

main :: Effect Unit
main = do
  doc <- liftEffect $ toDocument <$> (document =<< window)
  log =<< url doc
