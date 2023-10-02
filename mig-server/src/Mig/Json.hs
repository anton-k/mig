{-# LANGUAGE UndecidableInstances #-}

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

  -- * Json request body
  Body (..),

  -- * Json response
  Resp,
  RespOr,

  -- * re-exports
  module X,
) where

import Mig (
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
import Mig.Server.Common as X

-- response

newtype Resp a = Resp (Core.Resp Json a)
  deriving newtype (IsResp)

newtype RespOr err a = RespOr (Core.RespOr Json err a)
  deriving newtype (IsResp)

-- request

-- | Special case for ReqBody with JSON.
newtype Body a = Body a

instance (ToSchema a, ToRouteInfo b) => ToRouteInfo (Body a -> b) where
  toRouteInfo = toRouteInfo @(ReqBody Json a -> b)

instance (ToSchema a, FromJSON a, ToRoute b) => ToRoute (Body a -> b) where
  type RouteMonad (Body a -> b) = RouteMonad b

  toRouteFun f =
    (toRouteFun :: ((ReqBody Json a -> b) -> ServerFun (RouteMonad b)))
      (\(ReqBody a) -> f (Body a))

instance (ToSchema a, FromJSON a, ToRoute b) => ToServer (Body a -> b) where
  type ServerMonad (Body a -> b) = RouteMonad b
  toServer f =
    (toServer :: ((ReqBody Json a -> b) -> Server (RouteMonad b)))
      (\(ReqBody a) -> f (Body a))

instance (FromJSON a, ToMiddleware b) => ToMiddleware (Body a -> b) where
  type MiddlewareMonad (Body a -> b) = MiddlewareMonad b

  toMiddleware f =
    (toMiddleware :: ((ReqBody Json a -> b) -> Middleware (MiddlewareMonad b)))
      (\(ReqBody a) -> f (Body a))
