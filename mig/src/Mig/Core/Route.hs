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

  -- * Outputs
  SetStatus (..),
  AddHeaders (..),

  -- * Output methods
  Send (..),
  Get,
  Post,
  Put,
  Delete,
  IsMethod (..),
  GetMethod,
  PostMethod,
  PutMethod,
  DeleteMethod,
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
import GHC.TypeLits
import Mig.Core.Info
import Mig.Core.ServerFun
import Mig.Core.Types (ToByteStringResp (..), ToHtmlResp (..), ToJsonResp (..), ToTextResp (..), addRespHeaders, setRespStatus)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
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

instance (ToFormType a, ToRouteInfo b) => ToRouteInfo (FormBody a -> b) where
  toRouteInfo = setMediaInputType (MediaType "application/x-www-form-urlencoded") . addRouteInput (FormBodyInput (toFormType @a)) . toRouteInfo @b

instance (ToFormType a, FromForm a, ToRoute b) => ToRoute (FormBody a -> b) where
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

type Get ty m a = Send GetMethod ty m a
type Post ty m a = Send PostMethod ty m a
type Put ty m a = Send PutMethod ty m a
type Delete ty m a = Send DeleteMethod ty m a

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

newtype Send method ty m a = Send {unSend :: m a}

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m a) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (IsMethod method, ToSchema a) => ToRouteInfo (Send method Json m a) where
  toRouteInfo = setJsonMethod (toMethod @method) (toMediaType @Json) (toNamedSchema (Proxy @a))

instance (IsMethod method) => ToRouteInfo (Send method Json m Json.Value) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @Json)

instance (MonadIO m, ToTextResp a, IsMethod method) => ToRoute (Send method Text m a) where
  type RouteMonad (Send method Text m a) = m
  toRouteFun (Send a) = sendText a
  emptyRoute = Send (pure (error "No implementation"))

instance {-# OVERLAPPABLE #-} (MonadIO m, ToSchema a, ToJsonResp a, IsMethod method) => ToRoute (Send method Json m a) where
  type RouteMonad (Send method Json m a) = m
  toRouteFun (Send a) = sendJson a
  emptyRoute = Send (pure (error "No implementation"))

instance (MonadIO m, IsMethod method) => ToRoute (Send method Json m Json.Value) where
  type RouteMonad (Send method Json m Json.Value) = m
  toRouteFun (Send a) = sendJson a
  emptyRoute = Send (pure (error "No implementation"))

instance (MonadIO m, ToHtmlResp a, IsMethod method) => ToRoute (Send method Html m a) where
  type RouteMonad (Send method Html m a) = m
  toRouteFun (Send a) = sendHtml a
  emptyRoute = Send (pure (error "No implementation"))

instance (MonadIO m, ToByteStringResp a, IsMethod method) => ToRoute (Send method BL.ByteString m a) where
  type RouteMonad (Send method BL.ByteString m a) = m
  toRouteFun (Send a) = sendRaw a
  emptyRoute = Send (pure (error "No implementation"))

instance (MonadIO m, KnownSymbol sym, ToByteStringResp a, IsMethod method) => ToRoute (Send method (RawMedia sym) m a) where
  type RouteMonad (Send method (RawMedia sym) m a) = m
  toRouteFun (Send a) = sendRawMedia (fromString $ symbolVal (Proxy @sym)) a
  emptyRoute = Send (pure (error "No implementation"))

-- set status

data SetStatus a = SetStatus
  { status :: Status
  , content :: a
  }

-- add headers

data AddHeaders a = AddHeaders
  { headers :: ResponseHeaders
  , content :: a
  }

-- text response

instance (ToTextResp a) => ToTextResp (AddHeaders a) where
  toTextResp (AddHeaders headers content) = addRespHeaders headers (toTextResp content)

instance (ToTextResp a) => ToTextResp (SetStatus a) where
  toTextResp (SetStatus st content) =
    setRespStatus st (toTextResp content)

-- json response

instance (ToJsonResp a) => ToJsonResp (AddHeaders a) where
  toJsonResp (AddHeaders headers content) = addRespHeaders headers (toJsonResp content)

instance (ToJsonResp a) => ToJsonResp (SetStatus a) where
  toJsonResp (SetStatus st content) =
    setRespStatus st (toJsonResp content)

-- html response

instance (ToHtmlResp a) => ToHtmlResp (AddHeaders a) where
  toHtmlResp (AddHeaders headers content) = addRespHeaders headers (toHtmlResp content)

instance (ToHtmlResp a) => ToHtmlResp (SetStatus a) where
  toHtmlResp (SetStatus st content) =
    setRespStatus st (toHtmlResp content)

-- raw response

instance (ToByteStringResp a) => ToByteStringResp (AddHeaders a) where
  toByteStringResp (AddHeaders headers content) = addRespHeaders headers (toByteStringResp content)

instance (ToByteStringResp a) => ToByteStringResp (SetStatus a) where
  toByteStringResp (SetStatus st content) =
    setRespStatus st (toByteStringResp content)
