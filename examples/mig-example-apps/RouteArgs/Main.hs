{-| This example shows how to use input arguments for the server.

We can get input arguments with function that takes in
typed argument wrpapped in specific newtype which encodes the type of the input
-}
module Main (
  main,
) where

-- import Text and IO based server

import Data.Text.IO qualified as Text
import Mig.Json.IO

main :: IO ()
main = do
  putStrLn ("The route args server listens on port: " <> show port)
  runServer port routeArgs
  where
    port = 8085

-- | Let's define a server
routeArgs :: Server IO
routeArgs =
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
      , -- capture
        "mul" /. "*" /. "*" /. handleMul
      , -- json body as input
        "add-json" /. handleAddJson
      ]

-- | Simple getter
helloWorld :: Get Text
helloWorld = Send $ do
  logDebug "hello world route call"
  pure "Hello world!"

newtype TraceId = TraceId Text
  deriving newtype (FromHttpApiData, ToText, ToParamSchema)

{-| Using several inputs: header argument and required query
and using conditional output status
-}
handleSucc :: Header "Trace-Id" TraceId -> Query "value" Int -> Get (Response Int)
handleSucc (Header mTraceId) (Query n) = Send $ do
  logDebug "succ route call"
  mapM_ (logDebug . mappend "traceId: " . toText) mTraceId
  pure $ setStatus st $ okResponse (succ n)
  where
    st
      | n <= 0 = status400
      | otherwise = ok200

-- | Using optional query parameters and error as Either
handleSuccOpt :: Optional "value" Int -> Get (Either Error Int)
handleSuccOpt (Optional n) = Send $ do
  logDebug "succ optional route call"
  pure $ maybe (Left $ Error status400 "error") Right (succ <$> n)

{-| Using custom headers in response and several input query parameters.
Note that function can have any number of arguments.
We encode the input type with proper type-wrapper.
-}
handleAdd :: Query "a" Int -> Query "b" Int -> Get (Response Int)
handleAdd (Query a) (Query b) = Send $ do
  logDebug "add route call"
  pure $ addHeaders headers $ okResponse $ a + b
  where
    headers = [("args", "a, b")]

{-| Using capture as arguments. This route expects two arguments
captured in URL. For example:

> http://localhost:8085/hello/api/mul/3/100
-}
handleMul :: Capture "a" Int -> Capture "b" Int -> Get Int
handleMul (Capture a) (Capture b) = Send $ do
  logDebug "mul route call"
  pure (a * b)

-- | Using JSON as input and setting status for response
handleAddJson :: Body (Int, Int) -> Post (Response Int)
handleAddJson (Body (a, b)) = Send $ do
  logDebug "add route call"
  pure $ setStatus ok200 $ okResponse $ a + b

-- utils

logDebug :: Text -> IO ()
logDebug message =
  Text.putStrLn ("[DEBUG]: " <> message)
