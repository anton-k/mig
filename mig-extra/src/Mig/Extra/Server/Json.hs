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

import Mig.Client (FromClient (..), ToClient (..))
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

instance (FromJSON a, ToSchema a, ToPlugin b) => ToPlugin (Body a -> b) where
  toPluginInfo = toPluginInfo @(Core.Body Json a -> b)

  toPluginFun f =
    (toPluginFun :: ((Core.Body Json a -> b) -> PluginFun (Core.MonadOf b)))
      (\(Core.Body a) -> f (Body a))

-- client instances

instance (ToJSON a, ToClient b) => ToClient (Body a -> b) where
  toClient api = (\f -> \(Body b) -> f (Core.Body b)) $ toClient @(Core.Body Json a -> b) api
  clientArity = clientArity @(Core.Body Json a -> b)

instance (FromClient b) => FromClient (Body a -> b) where
  type ClientResult (Body a -> b) = ClientResult (Core.Body Json a -> b)
  fromClient f arg = fromClient @(Core.Body Json a -> b) (f . Body . fromBody) arg
    where
      fromBody (Core.Body a) = a
