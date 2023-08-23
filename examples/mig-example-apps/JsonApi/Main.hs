{-| Let's build a weather forecast API

We can query weather info. And also we can update the data.
User should get auth token that expires prior to making queries.
-}
module Main (
  main,
) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.OpenApi (toSchema)
import Data.Proxy
import Init (initEnv)
import Mig.Json.IO (runServer)
import Server (server)
import Types

main :: IO ()
main = do
  env <- initEnv
  putStrLn ("The weather forecast JSON API server listens on port: " <> show port)
  BSL.putStrLn $ encodePretty (toSchema (Proxy @(Timed WeatherData)))
  runServer port (server env)
  where
    port = 8085
