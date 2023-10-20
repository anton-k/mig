module Api (
  GetCounter,
  PutCounter,
  Routes (..),
  server,
) where

import Mig.Json

type GetCounter m = Get m (Resp Int)
type PutCounter m = Capture "arg" Int -> Put m (Resp ())

data Routes m = Routes
  { get :: GetCounter m
  , put :: PutCounter m
  }

server :: (MonadIO m) => Routes m -> Server m
server routes =
  "counter"
    /. [ "get" /. routes.get
       , "put" /. routes.put
       ]
