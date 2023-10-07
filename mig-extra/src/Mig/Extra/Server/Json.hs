{-# LANGUAGE UndecidableInstances #-}

-- | Json servers
module Mig.Extra.Server.Json (
  -- * Http verbs
  Get,
  Post,
  Put,
  Delete,
  Patch,
  Options,
  Head,
  Trace,

  -- * Json request body
  Body (..),

  -- * Json response
  Resp,
  RespOr,

  -- * re-exports
  module X,
) where

import Mig.Core (
  Delete,
  Get,
  Head,
  Options,
  Patch,
  Post,
  Put,
  Trace,
 )
import Mig.Core qualified as Core
import Mig.Extra.Server.Common as X

-- response

newtype Resp a = Resp (Core.Resp Json a)
  deriving newtype (IsResp, Functor)

newtype RespOr err a = RespOr (Core.RespOr Json err a)
  deriving newtype (IsResp, Functor)

-- request

-- | Special case for request Body with JSON.
newtype Body a = Body a

instance (ToSchema a, FromJSON a, ToRoute b) => ToRoute (Body a -> b) where
  toRouteInfo = toRouteInfo @(Core.Body Json a -> b)

  toRouteFun f =
    (toRouteFun :: ((Core.Body Json a -> b) -> ServerFun (Core.MonadOf b)))
      (\(Core.Body a) -> f (Body a))

instance (ToSchema a, FromJSON a, ToRoute b) => ToServer (Body a -> b) where
  toServer f =
    (toServer :: ((Core.Body Json a -> b) -> Server (Core.MonadOf b)))
      (\(Core.Body a) -> f (Body a))

instance (FromJSON a, ToSchema a, ToMiddleware b) => ToMiddleware (Body a -> b) where
  toMiddlewareInfo = toMiddlewareInfo @(Core.Body Json a -> b)

  toMiddlewareFun f =
    (toMiddlewareFun :: ((Core.Body Json a -> b) -> MiddlewareFun (Core.MonadOf b)))
      (\(Core.Body a) -> f (Body a))
