{-# Language OverloadedStrings #-}
-- | Most basic Hello world server.
-- It has only one route which outputs the greeting to the user
module Example.HelloWorld
  ( main
  ) where

import Mig.Json.IO
import Data.Text (Text)

-- | We can render the server and run it on port 8085.
-- It uses wai and warp.
main :: IO ()
main = runServer 8085 server

-- | Init simple hello world server which
-- replies on a single route
server :: Server IO
server =
  "api" /. "v1" /. "hello" /. hello

-- | Handler takes no inputs and marked as Get HTTP-request that returns Text.
hello :: Get Text
hello = Get $ pure "Hello World"
