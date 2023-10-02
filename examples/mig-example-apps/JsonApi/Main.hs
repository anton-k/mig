{-| Let's build a weather forecast API

We can query weather info. And also we can update the data.
User should get auth token that expires prior to making queries.
-}
module Main (
  main,
) where

import Control.Exception
import Data.Text qualified as Text
import Init (initEnv)
import Interface
import Mig.Json.IO (runServer)
import Server (server)

main :: IO ()
main = do
  env <- initEnv
  logInfo env greeting
  runServer port (server env)
    `finally` env.cleanup
  where
    port = 8085
    greeting = "The weather forecast JSON API server listens on port: " <> Text.pack (show port)
