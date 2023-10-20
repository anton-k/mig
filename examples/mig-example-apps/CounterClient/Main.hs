module Main (main) where

import Control.Concurrent
import Network.HTTP.Client qualified as Http
import Mig
import Mig.Client

import Server
import Client

-- Starts counter server in the background thread
-- and runs client script to interact with application.
--
-- The server logs are interleaved with client logs.
-- Server logs have prefix [INFO].
main :: IO ()
main = do
  appThread <- forkIO (runApp port)
  sleep 0.5
  putStrLn "Run the client script"
  runClientScript port
  putStrLn "Stop server and exit"
  killThread appThread
  where
    port = 8085

sleep :: Float -> IO ()
sleep n = threadDelay (floor $ 1000000 * n)

runApp :: Int -> IO ()
runApp port = do
  env <- initEnv
  putStrLn ("The counter server listens on port: " <> show port)
  runServer port . withSwagger def $ renderServer counterServer env

runClientScript :: Int -> IO ()
runClientScript port = do
  config <- initClientConfig port
  runClient' config $ do
    showInitState
    step 1
    step 2000
    step 22
  where
    newline = liftIO (putStrLn "")

    showInitState :: Client' ()
    showInitState = do
      newline
      liftIO $ putStrLn "Query init state"
      printRes "get" runGet

    step :: Int -> Client' ()
    step n = do
      newline
      liftIO $ putStrLn ("Increment by " <> show n <> " and show state")
      printRes ("put/" <> show n) (runPut n)
      printRes "get" runGet

    printRes :: (Show a) => String -> Client' a -> Client' ()
    printRes path act = do
      liftIO $ putStrLn $ "HTTP-client call at path: " <> path
      liftIO . print =<< act

initClientConfig :: Int -> IO ClientConfig
initClientConfig port =
  ClientConfig port <$> Http.newManager Http.defaultManagerSettings
