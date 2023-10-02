-- | To server class
module Mig.Core.Class.Server (
  (/.),
  ToServer (..),
) where

import Control.Monad.IO.Class
import Data.Kind
import Data.OpenApi (ToParamSchema, ToSchema)
import GHC.TypeLits
import Mig.Core.Api (Api)
import Mig.Core.Api qualified as Api
import Mig.Core.Class.MediaType (FromReqBody (..))
import Mig.Core.Class.Response (IsResp)
import Mig.Core.Class.Route
import Mig.Core.Server (Server (..))
import Web.HttpApiData

infixr 4 /.

(/.) :: (ToServer a) => Api.Path -> a -> Server (ServerMonad a)
(/.) path api
  | null path.unPath = toServer api
  | otherwise =
      case unServer (toServer api) of
        Api.WithPath rest a -> go rest a
        other -> go mempty other
  where
    go rest a = Server $ Api.WithPath (path <> rest) a

class ToServer a where
  type ServerMonad a :: Type -> Type
  toServer :: a -> Server (ServerMonad a)

-- identity

instance ToServer (Api (Route m)) where
  type ServerMonad (Api (Route m)) = m
  toServer = Server

instance ToServer (Server m) where
  type ServerMonad (Server m) = m
  toServer = id

-- outputs
instance (MonadIO m, IsResp a, IsMethod method) => ToServer (Send method m a) where
  type ServerMonad (Send method m a) = m
  toServer a = Server $ Api.HandleRoute (toRoute a)

-- inputs

instance (ToSchema a, FromReqBody media a, ToRoute b) => ToServer (ReqBody media a -> b) where
  type ServerMonad (ReqBody media a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Query sym a -> b) where
  type ServerMonad (Query sym a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Optional sym a -> b) where
  type ServerMonad (Optional sym a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Capture sym a -> b) where
  type ServerMonad (Capture sym a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Header sym a -> b) where
  type ServerMonad (Header sym a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (ToRoute b) => ToServer (PathInfo -> b) where
  type ServerMonad (PathInfo -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)
