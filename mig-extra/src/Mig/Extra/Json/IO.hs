module Mig.Extra.Json.IO (
  -- * re-exports
  module X,
) where

import Mig.Extra.IO as X (Delete, Get, Head, Options, Patch, Post, Put, Trace)
import Mig.Extra.Json as X (Body (..), Resp, RespOr)
import Mig.Extra.Server.Common as X
