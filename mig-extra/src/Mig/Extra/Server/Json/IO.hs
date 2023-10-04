-- | Json IO-based servers
module Mig.Extra.Server.Json.IO (
  -- * re-exports
  module X,
) where

import Mig.Extra.Server.Common as X
import Mig.Extra.Server.IO as X (Delete, Get, Head, Options, Patch, Post, Put, Trace)
import Mig.Extra.Server.Json as X (Body (..), Resp, RespOr)
