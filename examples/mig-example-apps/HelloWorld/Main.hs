{-# LANGUAGE OverloadedStrings #-}

-- \| Most basic Hello world server.
-- It has only two routes which output the text as JSON to the user
import Mig.Json.IO

-- | Starts server on port 8085.
main :: IO ()
main = runServer 8085 (withSwagger def server)

-- | The server definition with two routes
server :: Server IO
server =
  "api/v1"
    /. [ "hello" /. hello
       , "bye" /. bye
       ]

-- | The handler definition as a function
hello :: Get (Resp Text)
hello = pure $ ok "Hello World"

-- | The handler definition as a function with a query parameter to ask for the user name
bye :: Query "user" Text -> Get (Resp Text)
bye (Query name) = pure $ ok ("Goodbye " <> name)
