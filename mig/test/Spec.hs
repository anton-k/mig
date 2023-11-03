import Test.Api qualified as Api

import Test.Hspec

main :: IO ()
main =
  hspec $ Api.spec
