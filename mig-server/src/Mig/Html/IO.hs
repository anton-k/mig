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

type Get a = Send GetMethod Html IO a
type Post a = Send PostMethod Html IO a
type Put a = Send PutMethod Html IO a
type Delete a = Send DeleteMethod Html IO a
type Patch a = Send PatchMethod Html IO a
type Options a = Send OptionsMethod Html IO a
type Head a = Send HeadMethod Html IO a
type Trace a = Send TraceMethod Html IO a
