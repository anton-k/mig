{-| This example shows how to use input arguments for the server.

We can get input arguments with function that takes in
typed argument wrpapped in specific newtype which encodes the type of the input
-}
module Main (
  main,
) where

import Mig.Extra.Middleware.Trace qualified as Trace
import Mig.Json.IO

main :: IO ()
main = do
  putStrLn ("The route args server listens on port: " <> show port)
  runServer port (withSwagger def routeArgs)
  where
    port = 8085

-- | Let's define a server
routeArgs :: Server IO
routeArgs =
  withSwagger def $
    withTrace $
      "hello"
        /. "api"
        /. mconcat
          -- no args, constnat output
          [ "world" /. helloWorld
          , -- required query param and custom header
            "succ" /. handleSucc
          , -- optional query param
            "succ-opt" /. handleSuccOpt
          , -- several query params
            "add" /. handleAdd
          , -- query flag
            "add-if" /. handleAddIf
          , -- capture
            "mul" /. "*" /. "*" /. handleMul
          , -- json body as input
            "add-json" /. handleAddJson
          , -- return error
            "square-root" /. handleSquareRoot
          ]
  where
    withTrace = applyMiddleware (Trace.logHttp Trace.V2)

-- | Simple getter
helloWorld :: Get (Resp Text)
helloWorld = Send $ do
  pure $ ok "Hello world!"

newtype TraceId = TraceId Text
  deriving newtype (FromHttpApiData, ToHttpApiData, ToText, ToParamSchema)

{-| Using several inputs: header argument and required query
and using conditional output status
-}
handleSucc :: Header "Trace-Id" TraceId -> Query "value" Int -> Get (Resp Int)
handleSucc (Header traceId) (Query n) = Send $ do
  pure $ setHeader "Trace-Id" traceId $ setStatus st $ ok (succ n)
  where
    st
      | n <= 0 = status400
      | otherwise = ok200

-- | Using optional query parameters and error as RespOr.
handleSuccOpt :: Optional "value" Int -> Get (RespOr Text Int)
handleSuccOpt (Optional n) = Send $ do
  pure $ case n of
    Just val -> ok (succ val)
    Nothing -> bad status500 "error: no input"

{-| Using custom headers in response and several input query parameters.
Note that function can have any number of arguments.
We encode the input type with proper type-wrapper.
-}
handleAdd :: Query "a" Int -> Query "b" Int -> Get (Resp Int)
handleAdd (Query a) (Query b) = Send $ do
  pure $ addHeaders headers $ ok $ a + b
  where
    headers = [("args", "a, b")]

-- | Using query flag if flag is false returns 0
handleAddIf :: Query "a" Int -> Query "b" Int -> QueryFlag "perform" -> Get (Resp Int)
handleAddIf (Query a) (Query b) (QueryFlag addFlag) = Send $ do
  pure $
    ok $
      if addFlag
        then (a + b)
        else 0

{-| Using capture as arguments. This route expects two arguments
captured in URL. For example:

> http://localhost:8085/hello/api/mul/3/100
-}
handleMul :: Capture "a" Int -> Capture "b" Int -> Get (Resp Int)
handleMul (Capture a) (Capture b) = Send $ do
  pure $ ok (a * b)

data AddInput = AddInput
  { a :: Int
  , b :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Using JSON as input and setting status for response
handleAddJson :: Body AddInput -> Post (Resp Int)
handleAddJson (Body (AddInput a b)) = Send $ do
  pure $ setStatus ok200 $ ok $ a + b

handleSquareRoot :: Body Float -> Post (RespOr Text Float)
handleSquareRoot (Body arg) =
  Send $
    pure $
      if arg >= 0
        then ok (sqrt arg)
        else bad badRequest400 "Argument for square root should be non-negative"
