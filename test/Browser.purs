module Test.Browser where

import Effect (Effect)
import Prelude (Unit)
import Test.Spec.Mocha (runMocha)

import Test.Main (tests)

main :: Effect Unit
main = do
  -- Example
  -- example
  
  -- browser tests
  runMocha tests
