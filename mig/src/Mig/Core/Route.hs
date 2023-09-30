{-# LANGUAGE UndecidableInstances #-}

-- | Creation of routes from functions
module Mig.Core.Route (
  Route (..),
  ToRoute (..),
  toRoute,
  ServerFun (..),

  -- * Inputs
  Body (..),
  RawBody (..),
  Query (..),
  Optional (..),
  Capture (..),
  Header (..),
  OptionalHeader (..),
  FormBody (..),
  PathInfo (..),

  -- * Output methods
  Send (..),
  Get,
  Post,
  Put,
  Delete,
  Options,
  Head,
  Patch,
  Trace,

  -- ** Method tags
  IsMethod (..),
  GET,
  POST,
  PUT,
  DELETE,
  OPTIONS,
  HEAD,
  PATCH,
  TRACE,
) where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.Proxy
import Data.String
import Data.Text (Text)
import GHC.TypeLits
import Mig.Core.Info
import Mig.Core.ServerFun
import Mig.Core.Types (Error, Resp (..), RespBody (..), fromError, ok, setContent)
import Mig.Core.Types.MediaType (Json, MediaType (..), MimeRender (..), ToMediaType (..))
import Mig.Core.Types.Response (Response (..))
import Network.HTTP.Types.Method
import Web.FormUrlEncoded
import Web.HttpApiData

class (MonadIO (RouteMonad a), ToRouteInfo a) => ToRoute a where
  -- | Underyling server monad
  type RouteMonad a :: Type -> Type

  -- | Convert to route
  toRouteFun :: a -> ServerFun (RouteMonad a)

-- | Route contains API-info and how to run it
data Route m = Route
  { api :: RouteInfo
  , run :: ServerFun m
  }

toRoute :: forall a. (ToRoute a) => a -> Route (RouteMonad a)
toRoute a =
  Route
    { api = toRouteInfo @a emptyRouteInfo
    , run = toRouteFun a
    }

instance MapServerFun Route where
  mapServerFun f r = Route r.api (f r.run)

-------------------------------------------------------------------------------------
-- identity instances

instance ToRouteInfo (Route m) where
  toRouteInfo = id

instance (MonadIO m) => ToRoute (Route m) where
  type RouteMonad (Route m) = m
  toRouteFun = (.run)

instance (MonadIO m) => ToRoute (ServerFun m) where
  type RouteMonad (ServerFun m) = m
  toRouteFun = id

-------------------------------------------------------------------------------------
-- request inputs

newtype Body a = Body a

instance (ToSchema a, ToRouteInfo b) => ToRouteInfo (Body a -> b) where
  toRouteInfo = addRouteInput (ReqBodyInput (toMediaType @Json) (toSchemaDefs @a)) . toRouteInfo @b

instance (ToSchema a, FromJSON a, ToRoute b) => ToRoute (Body a -> b) where
  type RouteMonad (Body a -> b) = RouteMonad b

  toRouteFun f = withBody (toRouteFun . f . Body)

newtype RawBody = RawBody BL.ByteString

instance (ToRouteInfo b) => ToRouteInfo (RawBody -> b) where
  toRouteInfo = addRouteInput RawBodyInput . toRouteInfo @b

instance (ToRoute b) => ToRoute (RawBody -> b) where
  type RouteMonad (RawBody -> b) = RouteMonad b

  toRouteFun f = withRawBody (toRouteFun . f . RawBody)

newtype Query (sym :: Symbol) a = Query a

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Query sym a -> b) where
  toRouteInfo = addRouteInput (QueryInput (IsRequired True) name (toParamSchema (Proxy @a))) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Query sym a -> b) where
  type RouteMonad (Query sym a -> b) = RouteMonad b

  toRouteFun f = withQuery name (toRouteFun . f . Query)
    where
      name = fromString (symbolVal (Proxy @sym))

newtype Optional (sym :: Symbol) a = Optional (Maybe a)

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Optional sym a -> b) where
  toRouteInfo = addRouteInput (QueryInput (IsRequired False) name (toParamSchema (Proxy @a))) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Optional sym a -> b) where
  type RouteMonad (Optional sym a -> b) = RouteMonad b

  toRouteFun f = withOptional name (toRouteFun . f . Optional)
    where
      name = fromString (symbolVal (Proxy @sym))

newtype Capture (sym :: Symbol) a = Capture a

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Capture sym a -> b) where
  toRouteInfo = addRouteInput (CaptureInput name (toParamSchema (Proxy @a))) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Capture sym a -> b) where
  type RouteMonad (Capture sym a -> b) = RouteMonad b

  toRouteFun f = withCapture name (toRouteFun . f . Capture)
    where
      name = fromString (symbolVal (Proxy @sym))

newtype Header (sym :: Symbol) a = Header a

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Header sym a -> b) where
  toRouteInfo = addRouteInput (HeaderInput (IsRequired True) name (toParamSchema (Proxy @a))) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Header sym a -> b) where
  type RouteMonad (Header sym a -> b) = RouteMonad b

  toRouteFun f = withHeader name (toRouteFun . f . Header)
    where
      name = fromString (symbolVal (Proxy @sym))

newtype OptionalHeader (sym :: Symbol) a = OptionalHeader (Maybe a)

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (OptionalHeader sym a -> b) where
  toRouteInfo = addRouteInput (HeaderInput (IsRequired False) name (toParamSchema (Proxy @a))) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (OptionalHeader sym a -> b) where
  type RouteMonad (OptionalHeader sym a -> b) = RouteMonad b

  toRouteFun f = withOptionalHeader name (toRouteFun . f . OptionalHeader)
    where
      name = fromString (symbolVal (Proxy @sym))

newtype FormBody a = FormBody a

-- on form url encoded we should set media types for both input and output
-- to the value of "application/x-www-form-urlencoded"
instance (ToSchema a, ToRouteInfo b) => ToRouteInfo (FormBody a -> b) where
  toRouteInfo = setOutputMedia mediaType . addRouteInput (ReqBodyInput mediaType (toSchemaDefs @a)) . toRouteInfo @b
    where
      mediaType = MediaType "application/x-www-form-urlencoded"

instance (ToSchema a, FromForm a, ToRoute b) => ToRoute (FormBody a -> b) where
  type RouteMonad (FormBody a -> b) = RouteMonad b

  toRouteFun f = withFormBody (toRouteFun . f . FormBody)

-- | Reads current path info
newtype PathInfo = PathInfo [Text]

instance (ToRouteInfo b) => ToRouteInfo (PathInfo -> b) where
  toRouteInfo = toRouteInfo @b

instance (ToRoute b) => ToRoute (PathInfo -> b) where
  type RouteMonad (PathInfo -> b) = RouteMonad b
  toRouteFun f = withPathInfo (toRouteFun . f . PathInfo)

-------------------------------------------------------------------------------------
-- outputs

data GET
data POST
data PUT
data DELETE
data OPTIONS
data HEAD
data PATCH
data TRACE

type Get ty m a = Send GET ty m a
type Post ty m a = Send POST ty m a
type Put ty m a = Send PUT ty m a
type Delete ty m a = Send DELETE ty m a
type Options ty m a = Send OPTIONS ty m a
type Head ty m a = Send HEAD ty m a
type Patch ty m a = Send PATCH ty m a
type Trace ty m a = Send TRACE ty m a

class IsMethod a where
  toMethod :: Method

instance IsMethod GET where
  toMethod = methodGet

instance IsMethod POST where
  toMethod = methodPost

instance IsMethod PUT where
  toMethod = methodPut

instance IsMethod DELETE where
  toMethod = methodDelete

instance IsMethod OPTIONS where
  toMethod = methodOptions

instance IsMethod HEAD where
  toMethod = methodHead

instance IsMethod PATCH where
  toMethod = methodPatch

instance IsMethod TRACE where
  toMethod = methodTrace

newtype Send method ty m a = Send {unSend :: m a}

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m a) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m (Response a)) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m (Either Error a)) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (MonadIO m, MimeRender ty a, IsMethod method) => ToRoute (Send method ty m a) where
  type RouteMonad (Send method ty m a) = m
  toRouteFun (Send a) = sendResp $ ok @ty <$> a

instance (MonadIO m, MimeRender ty a, IsMethod method) => ToRoute (Send method ty m (Response a)) where
  type RouteMonad (Send method ty m (Response a)) = m
  toRouteFun (Send a) = sendResp $ (\resp -> Resp resp.status (resp.headers <> setContent media) (RawResp media $ mimeRender @ty resp.body)) <$> a
    where
      media = toMediaType @ty

instance {-# OVERLAPPABLE #-} (MonadIO m, MimeRender ty a, IsMethod method) => ToRoute (Send method ty m (Either Error a)) where
  type RouteMonad (Send method ty m (Either Error a)) = m
  toRouteFun (Send a) = sendResp $ fromError (ok @ty) <$> a