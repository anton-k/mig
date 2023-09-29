-- | Json servers
module Mig.Json (
  -- * Http verbs
  Get,
  Post,
  Put,
  Delete,
  Patch,
  Options,
  Head,
  Trace,

  -- * re-exports
  module X,
) where

import Mig.Server.Common as X

type Get m a = Send GET Json m a
type Post m a = Send POST Json m a
type Put m a = Send PUT Json m a
type Delete m a = Send DELETE Json m a
type Patch m a = Send PATCH Json m a
type Options m a = Send OPTIONS Json m a
type Head m a = Send HEAD Json m a
type Trace m a = Send TRACE Json m a
