module Mig.Extra.Html.IO (
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
  Body (..),
  module X,
) where

import Mig.Core (Body (..))
import Mig.Extra.Html (Resp (..), RespOr)
import Mig.Extra.Server.Common as X

type Get a = Send GET IO (Resp a)
type Post a = Send POST IO (Resp a)
type Put a = Send PUT IO (Resp a)
type Delete a = Send DELETE IO (Resp a)
type Patch a = Send PATCH IO (Resp a)
type Options a = Send OPTIONS IO (Resp a)
type Head a = Send HEAD IO (Resp a)
type Trace a = Send TRACE IO (Resp a)
