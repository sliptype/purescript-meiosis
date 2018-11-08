module Meiosis
       ( Driver
       , run
       , createSubjectDriver
       ) where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)
import RxJS.Observable (Observable)

type Driver a b = Observable a -> Observable b

foreign import run :: forall a b c d. (a -> b) -> Object (Driver c d) -> Effect Unit

foreign import createSubjectDriver :: forall a b. (Observable a -> Effect Unit) -> Driver a b
