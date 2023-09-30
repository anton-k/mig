-- | Json IO-based servers
module Mig.Json.IO (
  -- * Http verbs
  Get,
  Post,
  Put,
  Delete,
  Patch,
  Options,
  Head,
  Trace,

  -- * Json request body
  Body (..),

  -- * re-exports
  module X,
) where

import Mig.Json (Body (..))
import Mig.Server.Common as X

type Get a = Send GET Json IO a
type Post a = Send POST Json IO a
type Put a = Send PUT Json IO a
type Delete a = Send DELETE Json IO a
type Patch a = Send PATCH Json IO a
type Options a = Send OPTIONS Json IO a
type Head a = Send HEAD Json IO a
type Trace a = Send TRACE Json IO a
