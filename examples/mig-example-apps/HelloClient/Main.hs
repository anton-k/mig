module Main (
  main,
) where

import Data.ByteString.Lazy qualified as BL
import Mig
import Mig.Client
import Network.HTTP.Client qualified as Http

{-| Makes a call to hello world server with a client.
Run the hello-world example server in background
prior to execution of this file.
-}
main :: IO ()
main = do
  config <- ClientConfig port <$> Http.newManager Http.defaultManagerSettings
  print =<< runHello config
  where
    port = 8085

-------------------------------------------------------------------------------------
-- client definition

-- | Route API definition
type Hello m = Get m (Resp Json Text)

-- | Make it convenient to use
runHello :: ClientConfig -> IO (Either BL.ByteString Text)
runHello config = runClient' config $ getRespOrValue <$> fromClient hello

-- | Init client from API
hello :: Hello Client
hello = toClient server

{-| Note the recursive definition with hello route.
We use it as placeholder for undefined value.
It is ok to use it that way because client definition is derived
from the handler type
-}
server :: Server Client
server =
  "api/v1/hello" /. hello
