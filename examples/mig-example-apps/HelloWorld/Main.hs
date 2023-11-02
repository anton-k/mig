{-| Most basic Hello world server.
It has only one route which outputs the greeting to the user
-}
module Main (
  main,
) where

import Mig

{-| We can render the server and run it on port 8085.
It uses wai and warp.
-}
main :: IO ()
main = do
  putStrLn ("The hello world server listens on port: " <> show port)
  runServer port server
  where
    port = 8085

{-| Init simple hello world server which
replies on a single route
-}
server :: Server IO
server = "api/v1/hello" /. hello

-- | Handler takes no inputs and marked as Get HTTP-request that returns Text response as Json.
hello :: Get IO (Resp Json Text)
hello = pure $ ok "Hello World!"
