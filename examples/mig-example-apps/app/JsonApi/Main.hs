-- | Let's build a weather forecast API
--
-- We can query weather info. And also we can update the data.
-- User should get auth token that expires prior to making queries.
module Main
  ( main
  ) where

import Mig.Json.IO (runServer)
import Server (server)
import Init (initEnv)

main :: IO ()
main = do
  env <- initEnv
  putStrLn ("The weather forecast JSON API server listens on port: " <> show port)
  runServer port (server env)
  where
    port = 8085


