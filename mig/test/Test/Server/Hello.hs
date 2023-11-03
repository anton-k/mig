module Test.Server.Hello (spec) where

import Data.Aeson qualified as Json
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Mig.Core
import Mig.Core qualified as Request (Request (..))
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.HTTP.Types.Status (ok200)
import Test.Hspec

-- hello world server

server :: Server IO
server =
  "api/v1"
    /. [ "hello" /. handleHello
       , "bye" /. handleBye
       ]

handleHello :: Get IO (Resp Json Text)
handleHello = pure $ ok "hello"

handleBye :: Get IO (Resp Json Text)
handleBye = pure $ ok "bye"

-- tests

-- we use low-level representation of server as a function: Request -> m (Maybe Response)
-- to check server properties without launching in a full server environment
spec :: Spec
spec = describe "hello world server" $ do
  describe "plain route finder" $ specBy plainApiStrategy
  describe "tree route finder" $ specBy treeApiStrategy

specBy :: FindRoute nf IO -> Spec
specBy finder = do
  checkPositiveRoutes
  checkNegativeRoutes
  where
    serverFun :: ServerFun IO
    serverFun = fromServer finder server

    checkPositiveRoutes = do
      it "call routes (positive case)" $ do
        serverFun helloReq `shouldReturn` helloResp
        serverFun byeReq `shouldReturn` byeResp

    checkNegativeRoutes = do
      describe "negative cases" $ do
        it "wrong path" $ do
          serverFun emptyReq `shouldReturn` Nothing
          serverFun wrongPathReq `shouldReturn` Nothing
        it "wrong method" $ do
          serverFun (helloReq{Request.method = methodPost}) `shouldReturn` Nothing
        it "wrong output media type" $ do
          serverFun (helloReq{Request.headers = Map.fromList [("Accept", "text/html")]}) `shouldReturn` Nothing

    helloReq = emptyReq{path = ["api", "v1", "hello"]}
    helloResp = Just $ jsonResp @Text "hello"

    byeReq = emptyReq{path = ["api", "v1", "bye"]}
    byeResp = Just $ jsonResp @Text "bye"

    wrongPathReq = emptyReq{path = ["api", "v2", "hello"]}

emptyReq :: Request
emptyReq =
  Request
    { path = []
    , query = mempty
    , capture = mempty
    , headers = Map.fromList [("Accept", "application/json")]
    , method = methodGet
    , readBody = pure (Right "")
    , isSecure = False
    }

jsonResp :: (Json.ToJSON a) => a -> Response
jsonResp a =
  Response
    { status = ok200
    , headers = [("Content-Type", "application/json")]
    , body = RawResp "application/json" (Json.encode a)
    }
