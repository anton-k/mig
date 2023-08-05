{-# Language OverloadedStrings, DataKinds #-}
-- | This example shows how to use input arguments for the server.
--
-- We can get input arguments with function that takes in
-- typed argument wrpapped in specific newtype which encodes the type of the input
--
--
module Example.RouteArgs
  ( main
  ) where

-- import Text and IO based server
import Mig.Json.IO
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = runServer 8085 routeArgs

-- | Let's define a server
routeArgs :: Server IO
routeArgs =
  "hello" /. "api" /.
    mconcat
      -- no args, constnat output
      [ "world" /. helloWorld
      -- required query param and custom header
      , "succ" /. handleSucc
      -- optional query param
      , "succ-opt" /. handleSuccOpt
      -- several query params
      , "add" /. handleAdd
      -- capture
      , "mul" /. handleMul
      -- json body as input
      , "add-json" /. handleAddJson
      ]

-- | Simple getter
helloWorld :: Get Text
helloWorld = Get $ do
  logDebug "hello world route call"
  pure "Hello world!"

-- | Using several inputs: header argument and required query
-- and using conditional output status
handleSucc :: Header "Trace-Id" -> Query "value" Int -> Get (SetStatus Int)
handleSucc (Header mTraceId) (Query n) = Get $ do
  logDebug "succ route call"
  mapM_ (logDebug . mappend "traceId: " . Text.pack . show) mTraceId
  pure $ SetStatus st (succ n)
  where
    st
      | n <= 0 = status400
      | otherwise = ok200

-- | Using optional query parameters and error as Either
handleSuccOpt :: Optional "value" Int -> Get (Either (Error Text) Int)
handleSuccOpt (Optional n) = Get $ do
  logDebug "succ optional route call"
  pure $ maybe (Left $ Error status400 "error") Right (succ <$> n)

-- | Using custom headers in response and several input query parameters.
-- Note that function can have any number of arguments.
-- We encode the input type with proper type-wrapper.
handleAdd :: Query "a" Int -> Query "b" Int -> Get (AddHeaders Int)
handleAdd (Query a) (Query b) = Get $ do
  logDebug "add route call"
  pure $ AddHeaders headers $ a + b
  where
    headers = [("args", "a, b")]

-- | Using capture as arguments. This route expects two arguments
-- captured in URL. For example:
--
-- > http://localhost:8085/hello/api/mul/3/100
handleMul :: Capture Int -> Capture Int -> Get Int
handleMul (Capture a) (Capture b) = Get $ do
  logDebug "mul route call"
  pure (a * b)

-- | Using JSON as input and setting status for response
handleAddJson :: Body (Int, Int) -> Post (SetStatus Int)
handleAddJson (Body (a, b)) = Post $ do
  logDebug "add route call"
  pure $ SetStatus ok200 $ a + b

-- utils

logDebug :: Text -> IO ()
logDebug message =
  Text.putStrLn ("[DEBUG]: " <> message)

