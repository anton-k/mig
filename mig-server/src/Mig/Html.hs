{-# LANGUAGE UndecidableInstances #-}

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

  -- * Response
  Resp (..),
  RespOr,

  -- * re-exports
  Body (..),
  module X,
) where

import Mig.Core (Body (..))
import Mig.Core qualified as Core
import Mig.Server.Common as X

-- response

newtype Resp a = Resp (Core.Resp Html a)
  deriving newtype (IsResp)

type RespOr err a = Either (Resp err) (Resp a)

type Get m a = Send GET m (Resp a)
type Post m a = Send POST m (Resp a)
type Put m a = Send PUT m (Resp a)
type Delete m a = Send DELETE m (Resp a)
type Patch m a = Send PATCH m (Resp a)
type Options m a = Send OPTIONS m (Resp a)
type Head m a = Send HEAD m (Resp a)
type Trace m a = Send TRACE m (Resp a)
