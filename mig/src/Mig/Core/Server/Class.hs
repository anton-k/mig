-- | To server class
module Mig.Core.Server.Class (
  (/.),
  ToServer (..),
) where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Json (Value)
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Text (Text)
import GHC.TypeLits
import Mig.Core.Api (Api)
import Mig.Core.Api qualified as Api
import Mig.Core.Info (Json, RawMedia)
import Mig.Core.Route
import Mig.Core.Server (Server)
import Mig.Core.Types (ToByteStringResp, ToHtmlResp, ToJsonResp, ToTextResp)
import Text.Blaze.Html (Html)
import Web.FormUrlEncoded
import Web.HttpApiData

infixr 4 /.

(/.) :: (ToServer a) => Api.Path -> a -> Server (ServerMonad a)
(/.) path api = case toServer api of
  Api.WithPath rest a -> go rest a
  other -> go mempty other
  where
    go rest a = Api.WithPath (path <> rest) a

class ToServer a where
  type ServerMonad a :: Type -> Type
  toServer :: a -> Server (ServerMonad a)

-- identity

instance ToServer (Api (Route m)) where
  type ServerMonad (Api (Route m)) = m
  toServer = id

-- outputs

instance (MonadIO m, ToTextResp a, IsMethod method) => ToServer (Send method Text m a) where
  type ServerMonad (Send method Text m a) = m
  toServer a = Api.HandleRoute (toRoute a)

instance (MonadIO m, IsMethod method) => ToServer (Send method Json m Json.Value) where
  type ServerMonad (Send method Json m Json.Value) = m
  toServer a = Api.HandleRoute (toRoute a)

instance {-# OVERLAPPABLE #-} (MonadIO m, ToSchema a, ToJsonResp a, IsMethod method) => ToServer (Send method Json m a) where
  type ServerMonad (Send method Json m a) = m
  toServer a = Api.HandleRoute (toRoute a)

instance (MonadIO m, ToHtmlResp a, IsMethod method) => ToServer (Send method Html m a) where
  type ServerMonad (Send method Html m a) = m
  toServer a = Api.HandleRoute (toRoute a)

instance (MonadIO m, ToByteStringResp a, IsMethod method) => ToServer (Send method BL.ByteString m a) where
  type ServerMonad (Send method BL.ByteString m a) = m
  toServer a = Api.HandleRoute (toRoute a)

instance (MonadIO m, KnownSymbol sym, ToByteStringResp a, IsMethod method) => ToServer (Send method (RawMedia sym) m a) where
  type ServerMonad (Send method (RawMedia sym) m a) = m
  toServer a = Api.HandleRoute (toRoute a)

-- inputs

instance (ToSchema a, FromJSON a, ToRoute b) => ToServer (Body a -> b) where
  type ServerMonad (Body a -> b) = RouteMonad b
  toServer a = Api.HandleRoute (toRoute a)

instance (ToRoute b) => ToServer (RawBody -> b) where
  type ServerMonad (RawBody -> b) = RouteMonad b
  toServer a = Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Query sym a -> b) where
  type ServerMonad (Query sym a -> b) = RouteMonad b
  toServer a = Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Optional sym a -> b) where
  type ServerMonad (Optional sym a -> b) = RouteMonad b
  toServer a = Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Capture sym a -> b) where
  type ServerMonad (Capture sym a -> b) = RouteMonad b
  toServer a = Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Header sym a -> b) where
  type ServerMonad (Header sym a -> b) = RouteMonad b
  toServer a = Api.HandleRoute (toRoute a)

instance (ToSchema a, FromForm a, ToRoute b) => ToServer (FormBody a -> b) where
  type ServerMonad (FormBody a -> b) = RouteMonad b
  toServer a = Api.HandleRoute (toRoute a)

instance (ToRoute b) => ToServer (PathInfo -> b) where
  type ServerMonad (PathInfo -> b) = RouteMonad b
  toServer a = Api.HandleRoute (toRoute a)
