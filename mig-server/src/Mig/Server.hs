module Mig.Server (
  ServerConfig (..),
  toApplication,
  runServer,
  module X,
) where

import Mig.Internal.Server
import Mig.Internal.Wai (ServerConfig (..))
import Mig.Internal.Wai qualified as Wai
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import Mig.Internal.Api as X (Api, Path (..), PathItem, (/.))
import Mig.Internal.Info as X
import Mig.Internal.Route as X
import Mig.Internal.Server as X

toApplication :: ServerConfig -> Server IO -> Wai.Application
toApplication config server = Wai.toApplication config (fromServer server)

runServer :: Int -> Server IO -> IO ()
runServer port server = Warp.run port (toApplication config server)
  where
    config = ServerConfig{maxBodySize = Nothing}
