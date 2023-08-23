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

type Get m a = Send GetMethod Json m a
type Post m a = Send PostMethod Json m a
type Put m a = Send PutMethod Json m a
type Delete m a = Send DeleteMethod Json m a
type Patch m a = Send PatchMethod Json m a
type Options m a = Send OptionsMethod Json m a
type Head m a = Send HeadMethod Json m a
type Trace m a = Send TraceMethod Json m a
