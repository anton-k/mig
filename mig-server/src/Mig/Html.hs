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

type Get m a = Send GetMethod Html m a
type Post m a = Send PostMethod Html m a
type Put m a = Send PutMethod Html m a
type Delete m a = Send DeleteMethod Html m a
type Patch m a = Send PatchMethod Html m a
type Options m a = Send OptionsMethod Html m a
type Head m a = Send HeadMethod Html m a
type Trace m a = Send TraceMethod Html m a
