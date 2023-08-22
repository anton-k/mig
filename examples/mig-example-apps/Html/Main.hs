{-| Example on how to serve Html

We create a simple blog post site which offers two types of content:

* blog posts
* quotes

We can choose between the two. Also we can create new blog posts and list them.
all posts are stored in memory.
-}
module Main (
  main,
) where

import Init (initSite)
import Mig.Html.IO (runServer)
import Server (server)

-- run blog post server
main :: IO ()
main = do
  putStrLn ("The blog post server listens on port: " <> show port)
  site <- initSite
  runServer port $ server site
  where
    port = 8085
