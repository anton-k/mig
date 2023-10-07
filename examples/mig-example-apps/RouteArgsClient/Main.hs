module Main (main) where

import Mig.Client
import Mig.Json
import Network.HTTP.Client qualified as Http

{-| Makes a call to route-args server with a client.
Run the route-args example server in background
prior to execution of this file.
-}
main :: IO ()
main = do
  config <- ClientConfig port <$> Http.newManager Http.defaultManagerSettings
  runClient' config $ do
    printRes "api/hello/world" runHello
    printRes "api/succ" (runSucc (TraceId "trace-xyz") 10)
    printRes "api/succ-opt" (runSuccOpt Nothing)
    printRes "api/add" (runAdd 2 2)
    printRes "api/add-if" (runAddIf 2 2 False)
    printRes "api/mul" (runMul 2 6)
    printRes "api/add-json" (runAddJson (AddInput 101 30))
    printRes "api/square-root" (runSquareRoot 100)
    printRes "api/square-root negative" (runSquareRoot (-5))
  where
    port = 8085

    printRes :: (Show a) => String -> Client' a -> Client' ()
    printRes path act = do
      liftIO $ putStrLn $ "HTTP-client call at path: " <> path
      liftIO . print =<< act

-------------------------------------------------------------------------------------
-- client runners

-- | Make it convenient to use
runHello :: ClientOr Text
runHello = getRespOrValue <$> fromClient helloWorld

runSucc :: TraceId -> Int -> ClientOr Int
runSucc traceId arg = getRespOrValue <$> fromClient handleSucc traceId arg

runSuccOpt :: Maybe Int -> ClientOr Int
runSuccOpt mArg = getRespOrValue <$> fromClient handleSuccOpt mArg

runAdd :: Int -> Int -> ClientOr Int
runAdd a b = getRespOrValue <$> fromClient handleAdd a b

runAddIf :: Int -> Int -> Bool -> ClientOr Int
runAddIf a b flag = getRespOrValue <$> fromClient handleAddIf a b flag

runMul :: Int -> Int -> ClientOr Int
runMul a b = getRespOrValue <$> fromClient handleMul a b

runAddJson :: AddInput -> ClientOr Int
runAddJson args = getRespOrValue <$> fromClient handleAddJson args

runSquareRoot :: Float -> ClientOr Float
runSquareRoot args = getRespOrValue <$> fromClient handleSquareRoot args

-------------------------------------------------------------------------------------
-- client definition

helloWorld :: Get Client (Resp Text)
handleSucc :: Header "Trace-Id" TraceId -> Query "value" Int -> Get Client (Resp Int)
handleSuccOpt :: Optional "value" Int -> Get Client (RespOr Text Int)
handleAdd :: Query "a" Int -> Query "b" Int -> Get Client (Resp Int)
handleAddIf :: Query "a" Int -> Query "b" Int -> QueryFlag "perform" -> Get Client (Resp Int)
handleMul :: Capture "a" Int -> Capture "b" Int -> Get Client (Resp Int)
handleAddJson :: Body AddInput -> Post Client (Resp Int)
handleSquareRoot :: Body Float -> Post Client (RespOr Text Float)
helloWorld
  :| handleSucc
  :| handleSuccOpt
  :| handleAdd
  :| handleAddIf
  :| handleMul
  :| handleAddJson
  :| handleSquareRoot =
    toClient server

-- | Let's define a server
server :: Server Client
server =
  "api"
    /. mconcat
      -- no args, constnat output
      [ "hello/world" /. helloWorld
      , -- required query param and custom header
        "succ" /. handleSucc
      , -- optional query param
        "succ-opt" /. handleSuccOpt
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
      ]

data AddInput = AddInput
  { a :: Int
  , b :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype TraceId = TraceId Text
  deriving newtype (FromHttpApiData, ToHttpApiData, ToText, ToParamSchema)
