-- | Run mig-server with warp
module Mig.Server.Warp (
  runServer,
  runServer',
  ServerConfig (..),
  CacheConfig (..),
) where

import Mig.Core
import Mig.Core.Server.Cache
import Mig.Server.Wai
import Network.Wai.Handler.Warp qualified as Warp

runServer :: Int -> Server IO -> IO ()
runServer port server = Warp.run port (toApplication config server)
  where
    config = ServerConfig{maxBodySize = Nothing, cache = Nothing}

runServer' :: ServerConfig -> Int -> Server IO -> IO ()
runServer' config port server = Warp.run port (toApplication config server)
