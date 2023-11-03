import Test.Api qualified as Api
import Test.Server.Hello qualified as Server.Hello

import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Api.spec
    describe "server" $ do
      Server.Hello.spec
