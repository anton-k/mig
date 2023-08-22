module Main (
  main,
) where

import Data.Text (Text)
import Mig.Client
import Mig.Server
import Network.HTTP.Client

type Hello m = Capture "who" Text -> Capture "suffix" Text -> Get Json m Text
type Bye m = Optional "who" Text -> Post Json m Text

main :: IO ()
main = do
  config <- ClientConfig <$> newManager defaultManagerSettings
  resp <- runClient config $ unSend (hello (Capture "World") (Capture "!"))
  print resp

hello :: Hello Client
bye :: Bye Client
(hello, bye) = toClient server

server :: Server Client
server =
  "api"
    /. "v1"
    /. mconcat
      [ "hello" /. "*" /. "*" /. route hello
      , "bye" /. route bye
      ]