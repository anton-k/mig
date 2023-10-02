-- | IO-servers
module Mig.IO (
  -- * Http verbs
  Get,
  Post,
  Put,
  Delete,
  Patch,
  Options,
  Head,
  Trace,

  -- * Response
  Resp (..),
  RespOr,

  -- * re-exports
  module X,
) where

import Mig (Resp (..), RespOr)
import Mig.Server.Common as X

type Get a = Send GET IO a
type Post a = Send POST IO a
type Put a = Send PUT IO a
type Delete a = Send DELETE IO a
type Patch a = Send PATCH IO a
type Options a = Send OPTIONS IO a
type Head a = Send HEAD IO a
type Trace a = Send TRACE IO a
