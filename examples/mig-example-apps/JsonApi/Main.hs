{-| Let's build a weather forecast API

We can query weather info. And also we can update the data.
User should get auth token that expires prior to making queries.
-}
module Main (
  main,
) where

import Control.Exception
import Init (initEnv)
import Interface
import Mig.Json.IO (runServer)
import Server (server)

main :: IO ()
main = do
  env <- initEnv port
  env.proc.startup
  runServer port (server env)
    `finally` env.proc.cleanup
  where
    port = 8085
