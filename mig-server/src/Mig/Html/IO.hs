-- | Html IO-based servers
module Mig.Html.IO (
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

type Get a = Send GET Html IO a
type Post a = Send POST Html IO a
type Put a = Send PUT Html IO a
type Delete a = Send DELETE Html IO a
type Patch a = Send PATCH Html IO a
type Options a = Send OPTIONS Html IO a
type Head a = Send HEAD Html IO a
type Trace a = Send TRACE Html IO a
