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

import Control.Exception (finally)
import Data.Text qualified as Text
import Init (initSite)
import Interface
import Mig.Html.IO (runServer)
import Server (initServer)

-- run blog post server
main :: IO ()
main = do
  site <- initSite
  site.logInfo ("The blog post server listens on port: " <> Text.pack (show port))
  runServer port (initServer site)
    `finally` site.cleanup
  where
    port = 8085
