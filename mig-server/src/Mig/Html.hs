-- | Html servers
module Mig.Html (
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

type Get m a = Send GET Html m a
type Post m a = Send POST Html m a
type Put m a = Send PUT Html m a
type Delete m a = Send DELETE Html m a
type Patch m a = Send PATCH Html m a
type Options m a = Send OPTIONS Html m a
type Head m a = Send HEAD Html m a
type Trace m a = Send TRACE Html m a
