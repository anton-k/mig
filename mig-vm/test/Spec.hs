module Main (main) where

import Test.Hspec
import Test.Mig.Vm.Test qualified as Vm (spec)

main :: IO ()
main = hspec $ do
  Vm.spec
