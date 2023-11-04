{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Tests for various inputs for requests
module Test.Server.RouteArgs (spec) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Json
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Mig.Core
import Mig.Core qualified as Request (Request (..))
import Mig.Core qualified as Response (Response (..))
import Network.HTTP.Types.Method (Method, methodGet, methodPost)
import Network.HTTP.Types.Status (badRequest400, status201)
import Test.Hspec
import Test.Server.Common
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-------------------------------------------------------------------------------------
-- server

server :: Server IO
server =
  "api"
    /. [ "succ"
          /. [ "query" /. handleSuccQuery
             , "header" /. handleSuccHeader
             , "optional" /. handleSuccOpt
             , "optional-header" /. handleSuccHeaderOpt
             ]
       , -- several query params
         "add" /. handleAdd
       , -- query flag
         "add-if" /. handleAddIf
       , -- capture
         "mul" /. handleMul
       , -- json body as input
         "add-json" /. handleAddJson
       , -- return error
         "square-root" /. handleSquareRoot
       , "response"
          /. [ "status" /. handleStatus
             , "header" /. handleHeader
             , "error1" /. handleError1
             , "error2" /. handleError2
             ]
       ]

{-| Using several inputs: header argument and required query
and using conditional output status
-}
handleSuccQuery :: Query "value" Int -> Get IO (Resp Json Int)
handleSuccQuery (Query n) =
  pure $ ok (succ n)

newtype Value = Value Int
  deriving newtype (FromHttpApiData, ToHttpApiData, ToText, ToParamSchema)

{-| Using several inputs: header argument and required query
and using conditional output status
-}
handleSuccHeader :: Header "value" Value -> Get IO (Resp Json Int)
handleSuccHeader (Header (Value n)) = do
  pure $ ok (succ n)

-- | Using optional query parameters and error as RespOr.
handleSuccOpt :: Optional "value" Int -> Get IO (RespOr Json Text Int)
handleSuccOpt (Optional n) = do
  pure $ ok $ maybe 0 succ n

-- | Using optional header parameters and error as RespOr.
handleSuccHeaderOpt :: OptionalHeader "value" Int -> Get IO (RespOr Json Text Int)
handleSuccHeaderOpt (OptionalHeader n) = do
  pure $ ok $ maybe 0 succ n

{-| Using custom headers in response and several input query parameters.
Note that function can have any number of arguments.
We encode the input type with proper type-wrapper.
-}
handleAdd :: Query "a" Int -> Query "b" Int -> Get IO (Resp Json Int)
handleAdd (Query a) (Query b) = do
  pure $ ok $ a + b

-- | Using query flag if flag is false returns 0
handleAddIf :: Query "a" Int -> Query "b" Int -> QueryFlag "perform" -> Get IO (Resp Json Int)
handleAddIf (Query a) (Query b) (QueryFlag addFlag) = do
  pure $
    ok $
      if addFlag
        then (a + b)
        else 0

{-| Using capture as arguments. This route expects two arguments
captured in URL. For example:

> http://localhost:8085/hello/api/mul/3/100
-}
handleMul :: Capture "a" Int -> Capture "b" Int -> Get IO (Resp Json Int)
handleMul (Capture a) (Capture b) = do
  pure $ ok (a * b)

data AddInput = AddInput
  { a :: Int
  , b :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Using JSON as input
handleAddJson :: Body Json AddInput -> Post IO (Resp Json Int)
handleAddJson (Body (AddInput a b)) = do
  pure $ ok $ a + b

handleSquareRoot :: Body Json Float -> Post IO (RespOr Json Text Float)
handleSquareRoot (Body arg) =
  pure $
    if arg >= 0
      then ok (sqrt arg)
      else bad badRequest400 sqrtError

handleStatus :: Get IO (Resp Json Text)
handleStatus = pure $ setStatus status201 $ ok "Status is 201"

handleHeader :: Capture "name" Text -> Capture "value" Text -> Get IO (Resp Json Text)
handleHeader (Capture name) (Capture value) =
  pure $ setHeader (fromString $ Text.unpack name) value $ ok "Set custom header"

handleError1 :: Get IO (Resp Json Text)
handleError1 = pure $ bad badRequest400 badRequestError

handleError2 :: Capture "value" Int -> Get IO (RespOr Json Text Int)
handleError2 (Capture n)
  | n > 0 = pure $ ok n
  | otherwise = pure $ bad badRequest400 badRequestError

badRequestError :: Text
badRequestError = "Error: bad request"

sqrtError :: Text
sqrtError = "Argument for square root should be non-negative"

-------------------------------------------------------------------------------------
-- test cases

-- we use low-level representation of server as a function: Request -> m (Maybe Response)
-- to check server properties without launching in a full server environment
spec :: Spec
spec = describe "route args server: check route inputs" $ do
  describe "plain route finder" $ specBy plainApiStrategy
  describe "tree route finder" $ specBy treeApiStrategy

specBy :: FindRoute normalForm IO -> Spec
specBy finder = do
  describe "request" $ do
    checkQuery
    checkOptionalQuery
    checkHeader
    checkOptionalHeader
    checkQueryFlag
    checkCapture
    checkBody
  describe "response" $ do
    checkStatus
    checkHeaders
    checkErrors
  where
    serverFun :: ServerFun IO
    serverFun = fromServer finder server

    shouldReq :: forall a. (Json.FromJSON a, Show a, Eq a) => Request -> Maybe a -> Expectation
    shouldReq req expected =
      fmap (parseResp @a =<<) (serverFun req) `shouldReturn` expected

    toQuery :: forall a. (Json.ToJSON a) => ByteString -> a -> QueryMap
    toQuery name val = Map.singleton name (Just $ BL.toStrict $ Json.encode @a val)

    jsonHeaders :: HeaderMap
    jsonHeaders = Map.fromList [("accept", "application/json"), ("content-type", "application/json")]

    -- queries

    checkQuery :: Spec
    checkQuery =
      describe "query" $ do
        it "one query" $ shouldReq @Int queryReq (Just 2)
        it "missing query" $ shouldReq @Int (queryReq{query = mempty}) Nothing
        it "two queries" $ shouldReq @Int (twoQueryReq 2 2) (Just 4)

    queryReq :: Request
    queryReq =
      emptyReq
        { path = ["api", "succ", "query"]
        , query = toQuery @Int "value" 1
        }

    twoQueryReq :: Int -> Int -> Request
    twoQueryReq a b =
      emptyReq
        { path = ["api", "add"]
        , query = toQuery "a" a <> toQuery "b" b
        }

    -- optional query

    checkOptionalQuery :: Spec
    checkOptionalQuery =
      describe "optional query" $ do
        it "with query" $ shouldReq @Int optionalQueryReq (Just 2)
        it "no query (ok, default case)" $ shouldReq @Int (optionalQueryReq{query = mempty}) (Just 0)

    optionalQueryReq :: Request
    optionalQueryReq =
      emptyReq
        { path = ["api", "succ", "optional"]
        , query = toQuery @Int "value" 1
        }

    -- query flag

    checkQueryFlag :: Spec
    checkQueryFlag =
      describe "query flag" $ do
        it "flag true" $ shouldReq @Int (queryFlagReq (Just True) 2 3) (Just 5)
        it "flag false" $ shouldReq @Int (queryFlagReq (Just False) 2 3) (Just 0)
        it "flag missing" $ shouldReq @Int (queryFlagReq Nothing 2 3) (Just 0)

    queryFlagReq :: Maybe Bool -> Int -> Int -> Request
    queryFlagReq mFlag a b =
      emptyReq
        { path = ["api", "add-if"]
        , query = mconcat [toQuery "a" a, toQuery "b" b] <> maybe mempty (toQuery "perform") mFlag
        }

    -- headers

    checkHeader :: Spec
    checkHeader =
      describe "header" $ do
        describe "input header" $ do
          it "positive case" $ shouldReq @Int headerReq (Just 2)
          it "missing header" $ shouldReq @Int (headerReq{Request.headers = mempty}) Nothing

    headerReq :: Request
    headerReq =
      emptyReq
        { path = ["api", "succ", "header"]
        , Request.headers = Map.singleton "value" (BL.toStrict $ Json.encode @Int 1)
        }

    -- optional headers

    checkOptionalHeader :: Spec
    checkOptionalHeader =
      describe "optional header" $ do
        it "with header" $ shouldReq @Int optionalHeaderReq (Just 2)
        it "no header (ok, default case)" $ shouldReq @Int (optionalHeaderReq{Request.headers = mempty}) (Just 0)

    optionalHeaderReq :: Request
    optionalHeaderReq =
      emptyReq
        { path = ["api", "succ", "optional-header"]
        , Request.headers = Map.singleton "value" (BL.toStrict $ Json.encode @Int 1)
        }

    -- captures

    checkCapture :: Spec
    checkCapture =
      describe "capture" $ do
        it "positive case" $ shouldReq @Int (captureReq [2, 3]) (Just 6)
        it "not enough  captures" $ shouldReq @Int ((captureReq [2]){capture = mempty}) Nothing
        it "too many captures" $ shouldReq @Int ((captureReq [2, 3, 4]){capture = mempty}) Nothing
        it "missing captures" $ shouldReq @Int ((captureReq []){capture = mempty}) Nothing

    captureReq :: [Int] -> Request
    captureReq args =
      emptyReq
        { path = ["api", "mul"] <> fmap (Text.pack . show) args
        }

    -- body

    checkBody :: Spec
    checkBody =
      describe "body" $ do
        it "positive case 1" $ shouldReq @Int (bodyReq methodPost 2 3) (Just 5)
        it "positive case 2" $ shouldReq @Float (sqrtBodyReq 9) (Just 3)
        it "no body" $ shouldReq @Int noBodyReq Nothing
        it "wrong method" $ shouldReq @Int (bodyReq methodGet 2 2) Nothing
        it "bad argument" $ shouldReq @Text (sqrtBodyReq (-9)) (Just sqrtError)

    bodyReq :: Method -> Int -> Int -> Request
    bodyReq reqMethod a b =
      emptyReq
        { path = ["api", "add-json"]
        , method = reqMethod
        , readBody = pure $ Right $ Json.encode $ AddInput a b
        , Request.headers = jsonHeaders
        }

    noBodyReq :: Request
    noBodyReq =
      emptyReq
        { path = ["api", "add-json"]
        , method = methodPost
        , Request.headers = jsonHeaders
        }

    sqrtBodyReq :: Float -> Request
    sqrtBodyReq a =
      emptyReq
        { path = ["api", "square-root"]
        , method = methodPost
        , readBody = pure $ Right $ Json.encode a
        , Request.headers = jsonHeaders
        }

    -- response status

    checkStatus :: Spec
    checkStatus =
      describe "status" $ do
        it "can set result status" $
          (fmap (.status) <$> serverFun statusReq) `shouldReturn` Just status201
        it "can set error status" $
          (fmap (.status) <$> serverFun (sqrtBodyReq (-1))) `shouldReturn` Just badRequest400

    statusReq :: Request
    statusReq =
      emptyReq
        { path = ["api", "response", "status"]
        }

    -- response headers

    checkHeaders :: Spec
    checkHeaders =
      describe "headers" $
        it "can set headers" $
          shouldHeader "foo" "bar"
      where
        shouldHeader name value =
          fmap (any (== header) . Response.headers) <$> serverFun (customHeaderReq name value)
            `shouldReturn` Just True
          where
            header = (fromString $ Text.unpack name, fromString $ Text.unpack value)

    customHeaderReq :: Text -> Text -> Request
    customHeaderReq name value =
      emptyReq
        { path = ["api", "response", "header", name, value]
        }

    -- response errors

    checkErrors :: Spec
    checkErrors =
      describe "custom errors" $ do
        it "error has the same type as result" $
          shouldBadReq (customErrorReq ["error1"])
        it "error has different type" $
          shouldBadReq (customErrorReq ["error2", "0"])
        it "no error on positive input" $
          shouldReq @Int (customErrorReq ["error2", "1"]) (Just 1)
      where
        shouldBadReq req =
          fmap (maybe False isBadReq) (serverFun req) `shouldReturn` True

        isBadReq resp =
          resp.status == badRequest400
            && parseResp @Text resp == Just badRequestError

    customErrorReq :: [Text] -> Request
    customErrorReq args =
      emptyReq
        { path = ["api", "response"] <> args
        }
