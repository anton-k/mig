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

  -- * re-exports
  module X,
) where

import Mig.Server.Common as X

-- response

type Get m a = Send GET Json m a
type Post m a = Send POST Json m a
type Put m a = Send PUT Json m a
type Delete m a = Send DELETE Json m a
type Patch m a = Send PATCH Json m a
type Options m a = Send OPTIONS Json m a
type Head m a = Send HEAD Json m a
type Trace m a = Send TRACE Json m a

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
