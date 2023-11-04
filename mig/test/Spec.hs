import Test.Api qualified as Api
import Test.Server qualified as Server

import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Api.spec
    Server.spec
