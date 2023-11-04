module Test.Server (spec) where

import Test.Hspec
import Test.Server.Counter qualified as Counter
import Test.Server.Hello qualified as Hello
import Test.Server.RouteArgs qualified as RouteArgs

spec :: Spec
spec = describe "server" $ do
  Hello.spec
  RouteArgs.spec
  Counter.spec
