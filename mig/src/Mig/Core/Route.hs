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
  GetMethod,
  PostMethod,
  PutMethod,
  DeleteMethod,
  OptionsMethod,
  HeadMethod,
  PatchMethod,
  TraceMethod,
) where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.OpenApi.Internal.Schema (toNamedSchema)
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.TypeLits
import Mig.Core.Info
import Mig.Core.ServerFun
import Mig.Core.Types (Resp, Response (..), ToByteStringResp (..), ToHtmlResp (..), ToJsonResp (..), ToTextResp (..), addRespHeaders, fromResponse)
import Network.HTTP.Types.Method
import Text.Blaze.Html (Html)
import Web.FormUrlEncoded
import Web.HttpApiData

class (MonadIO (RouteMonad a), ToRouteInfo a) => ToRoute a where
  -- | Underyling server monad
  type RouteMonad a :: Type -> Type

  -- | Convert to route
  toRouteFun :: a -> ServerFun (RouteMonad a)

  -- | route that produce nothing
  emptyRoute :: a

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
  emptyRoute = Route emptyRouteInfo emptyRoute

instance (MonadIO m) => ToRoute (ServerFun m) where
  type RouteMonad (ServerFun m) = m
  toRouteFun = id
  emptyRoute = ServerFun $ const $ pure Nothing

-------------------------------------------------------------------------------------
-- request inputs

newtype Body a = Body a

instance (ToSchema a, ToRouteInfo b) => ToRouteInfo (Body a -> b) where
  toRouteInfo = setMediaInputType (toMediaType @Json) . addRouteInput (BodyJsonInput (toNamedSchema (Proxy @a))) . toRouteInfo @b

instance (ToSchema a, FromJSON a, ToRoute b) => ToRoute (Body a -> b) where
  type RouteMonad (Body a -> b) = RouteMonad b

  toRouteFun f = withBody (toRouteFun . f . Body)

  emptyRoute = const emptyRoute

newtype RawBody = RawBody BL.ByteString

instance (ToRouteInfo b) => ToRouteInfo (RawBody -> b) where
  toRouteInfo = addRouteInput RawBodyInput . toRouteInfo @b

instance (ToRoute b) => ToRoute (RawBody -> b) where
  type RouteMonad (RawBody -> b) = RouteMonad b

  toRouteFun f = withRawBody (toRouteFun . f . RawBody)

  emptyRoute = const emptyRoute

newtype Query (sym :: Symbol) a = Query a

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Query sym a -> b) where
  toRouteInfo = addRouteInput (QueryInput name (toParamSchema (Proxy @a))) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Query sym a -> b) where
  type RouteMonad (Query sym a -> b) = RouteMonad b

  toRouteFun f = withQuery name (toRouteFun . f . Query)
    where
      name = fromString (symbolVal (Proxy @sym))

  emptyRoute = const emptyRoute

newtype Optional (sym :: Symbol) a = Optional (Maybe a)

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Optional sym a -> b) where
  toRouteInfo = addRouteInput (QueryInput name (toParamSchema (Proxy @a))) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Optional sym a -> b) where
  type RouteMonad (Optional sym a -> b) = RouteMonad b

  toRouteFun f = withOptional name (toRouteFun . f . Optional)
    where
      name = fromString (symbolVal (Proxy @sym))

  emptyRoute = const emptyRoute

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

  emptyRoute = const emptyRoute

newtype Header (sym :: Symbol) a = Header (Maybe a)

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Header sym a -> b) where
  toRouteInfo = addRouteInput (HeaderInput name (toParamSchema (Proxy @a))) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Header sym a -> b) where
  type RouteMonad (Header sym a -> b) = RouteMonad b

  toRouteFun f = withHeader name (toRouteFun . f . Header)
    where
      name = fromString (symbolVal (Proxy @sym))

  emptyRoute = const emptyRoute

newtype FormBody a = FormBody a

instance (ToSchema a, ToRouteInfo b) => ToRouteInfo (FormBody a -> b) where
  toRouteInfo = setMediaInputType (MediaType "application/x-www-form-urlencoded") . addRouteInput (FormBodyInput (toNamedSchema (Proxy @a))) . toRouteInfo @b

instance (ToSchema a, FromForm a, ToRoute b) => ToRoute (FormBody a -> b) where
  type RouteMonad (FormBody a -> b) = RouteMonad b

  toRouteFun f = withFormBody (toRouteFun . f . FormBody)

  emptyRoute = const emptyRoute

-- | Reads current path info
newtype PathInfo = PathInfo [Text]

instance (ToRouteInfo b) => ToRouteInfo (PathInfo -> b) where
  toRouteInfo = toRouteInfo @b

instance (ToRoute b) => ToRoute (PathInfo -> b) where
  type RouteMonad (PathInfo -> b) = RouteMonad b
  toRouteFun f = withPathInfo (toRouteFun . f . PathInfo)
  emptyRoute = const emptyRoute

-------------------------------------------------------------------------------------
-- outputs

data GetMethod
data PostMethod
data PutMethod
data DeleteMethod
data OptionsMethod
data HeadMethod
data PatchMethod
data TraceMethod

type Get ty m a = Send GetMethod ty m a
type Post ty m a = Send PostMethod ty m a
type Put ty m a = Send PutMethod ty m a
type Delete ty m a = Send DeleteMethod ty m a
type Options ty m a = Send OptionsMethod ty m a
type Head ty m a = Send HeadMethod ty m a
type Patch ty m a = Send PatchMethod ty m a
type Trace ty m a = Send TraceMethod ty m a

class IsMethod a where
  toMethod :: Method

instance IsMethod GetMethod where
  toMethod = methodGet

instance IsMethod PostMethod where
  toMethod = methodPost

instance IsMethod PutMethod where
  toMethod = methodPut

instance IsMethod DeleteMethod where
  toMethod = methodDelete

instance IsMethod OptionsMethod where
  toMethod = methodOptions

instance IsMethod HeadMethod where
  toMethod = methodHead

instance IsMethod PatchMethod where
  toMethod = methodPatch

instance IsMethod TraceMethod where
  toMethod = methodTrace

newtype Send method ty m a = Send {unSend :: m a}

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m a) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m (Response a)) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (IsMethod method, ToSchema a) => ToRouteInfo (Send method Json m a) where
  toRouteInfo = setJsonMethod (toMethod @method) (toMediaType @Json) (toNamedSchema (Proxy @a))

instance {-# OVERLAPPABLE #-} (IsMethod method, ToSchema a) => ToRouteInfo (Send method Json m (Response a)) where
  toRouteInfo = setJsonMethod (toMethod @method) (toMediaType @Json) (toNamedSchema (Proxy @a))

instance (IsMethod method) => ToRouteInfo (Send method Json m Json.Value) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @Json)

instance (IsMethod method) => ToRouteInfo (Send method Json m (Response Json.Value)) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @Json)

instance {-# OVERLAPPABLE #-} (MonadIO m, ToTextResp a, IsMethod method) => ToRoute (Send method Text m a) where
  type RouteMonad (Send method Text m a) = m
  toRouteFun (Send a) = sendResp $ toTextResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance (MonadIO m, ToTextResp a, IsMethod method) => ToRoute (Send method Text m (Response a)) where
  type RouteMonad (Send method Text m (Response a)) = m
  toRouteFun (Send a) = sendResp $ fromResponse toTextResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance {-# OVERLAPPABLE #-} (MonadIO m, ToSchema a, ToJsonResp a, IsMethod method) => ToRoute (Send method Json m a) where
  type RouteMonad (Send method Json m a) = m
  toRouteFun (Send a) = sendResp $ toJsonResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance {-# OVERLAPPABLE #-} (MonadIO m, ToSchema a, ToJsonResp a, IsMethod method) => ToRoute (Send method Json m (Response a)) where
  type RouteMonad (Send method Json m (Response a)) = m
  toRouteFun (Send a) = sendResp $ fromResponse toJsonResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance (MonadIO m, IsMethod method) => ToRoute (Send method Json m Json.Value) where
  type RouteMonad (Send method Json m Json.Value) = m
  toRouteFun (Send a) = sendResp $ toJsonResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance {-# OVERLAPPABLE #-} (MonadIO m, ToHtmlResp a, IsMethod method) => ToRoute (Send method Html m a) where
  type RouteMonad (Send method Html m a) = m
  toRouteFun (Send a) = sendResp $ toHtmlResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance (MonadIO m, ToHtmlResp a, IsMethod method) => ToRoute (Send method Html m (Response a)) where
  type RouteMonad (Send method Html m (Response a)) = m
  toRouteFun (Send a) = sendResp $ fromResponse toHtmlResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance {-# OVERLAPPABLE #-} (MonadIO m, ToByteStringResp a, IsMethod method) => ToRoute (Send method BL.ByteString m a) where
  type RouteMonad (Send method BL.ByteString m a) = m
  toRouteFun (Send a) = sendResp $ toByteStringResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance (MonadIO m, ToByteStringResp a, IsMethod method) => ToRoute (Send method BL.ByteString m (Response a)) where
  type RouteMonad (Send method BL.ByteString m (Response a)) = m
  toRouteFun (Send a) = sendResp $ fromResponse toByteStringResp <$> a
  emptyRoute = Send (pure (error "No implementation"))

instance {-# OVERLAPPABLE #-} (MonadIO m, KnownSymbol sym, ToByteStringResp a, IsMethod method) => ToRoute (Send method (RawMedia sym) m a) where
  type RouteMonad (Send method (RawMedia sym) m a) = m
  toRouteFun (Send a) = sendResp $ toRawByteStringResp (fromString $ symbolVal (Proxy @sym)) <$> a
  emptyRoute = Send (pure (error "No implementation"))

toRawByteStringResp :: (ToByteStringResp a) => MediaType -> a -> Resp
toRawByteStringResp (MediaType mediaType) =
  addRespHeaders [("Content-Type", Text.encodeUtf8 mediaType)] . toByteStringResp

instance (MonadIO m, KnownSymbol sym, ToByteStringResp a, IsMethod method) => ToRoute (Send method (RawMedia sym) m (Response a)) where
  type RouteMonad (Send method (RawMedia sym) m (Response a)) = m
  toRouteFun (Send a) = sendResp $ fromResponse (toRawByteStringResp (fromString $ symbolVal (Proxy @sym))) <$> a
  emptyRoute = Send (pure (error "No implementation"))
