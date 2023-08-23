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

  -- * re-exports
  module X,
) where

import Mig.Server.Common as X

type Get a = Send GetMethod Json IO a
type Post a = Send PostMethod Json IO a
type Put a = Send PutMethod Json IO a
type Delete a = Send DeleteMethod Json IO a
type Patch a = Send PatchMethod Json IO a
type Options a = Send OptionsMethod Json IO a
type Head a = Send HeadMethod Json IO a
type Trace a = Send TraceMethod Json IO a
