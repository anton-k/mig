-- | Most basic Hello world server.
-- It has only one route which outputs the greeting to the user
module Main
  ( main
  ) where

-- import Mig.Json.IO
import Data.Text (Text)
import Mig.Server
import Mig.Internal.Api (toNormalApi)
import Text.Show.Pretty

-- | We can render the server and run it on port 8085.
-- It uses wai and warp.
main :: IO ()
main = do
  putStrLn ("The hello world server listens on port: " <> show port)
  pPrint (fmap (.api) $ toNormalApi server)
  runServer port server
  where
    port = 8085

-- | Init simple hello world server which
-- replies on a single route
server :: Server IO
server =
  "api" /. "v1" /. mconcat ["hello" /. "who" /$ route hello, "bye" /. route bye]

-- | Handler takes no inputs and marked as Get HTTP-request that returns Text.
hello :: Capture "who" Text -> Get Json IO Text
hello (Capture who) = Get $ pure $ mappend "Hello " who

bye :: Optional "who" Text -> Post Json IO Text
bye (Optional mWho) = Post $ pure $ mappend "Hello " $ maybe "World" id mWho

