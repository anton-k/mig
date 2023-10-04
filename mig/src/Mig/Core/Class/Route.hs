{-# LANGUAGE UndecidableInstances #-}

-- | Creation of routes from functions
module Mig.Core.Class.Route (
  Route (..),
  ToRoute (..),
  toRoute,
  ServerFun,

  -- * Inputs
  Body (..),
  Query (..),
  QueryFlag (..),
  Optional (..),
  Capture (..),
  Header (..),
  OptionalHeader (..),
  PathInfo (..),
  RawRequest (..),
  IsSecure (..),

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

  -- ** Media
) where

import Control.Monad.IO.Class
import Data.Kind
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.Proxy
import Data.String
import Data.Text (Text)
import GHC.TypeLits
import Mig.Core.Class.MediaType
import Mig.Core.Class.Response (IsResp (..))
import Mig.Core.ServerFun
import Mig.Core.Types
import Network.HTTP.Types.Method
import Web.HttpApiData

class (MonadIO (RouteMonad a)) => ToRoute a where
  -- | Underyling server monad
  type RouteMonad a :: Type -> Type

  -- | Update API info
  toRouteInfo :: RouteInfo -> RouteInfo

  -- | Convert to route
  toRouteFun :: a -> ServerFun (RouteMonad a)

-- | Route contains API-info and how to run it
data Route m = Route
  { info :: RouteInfo
  -- ^ definition of the API (to use it in OpenApi or clients)
  , run :: ServerFun m
  -- ^ how to run a server
  }

toRoute :: forall a. (ToRoute a) => a -> Route (RouteMonad a)
toRoute a =
  Route
    { info = toRouteInfo @a emptyRouteInfo
    , run = toRouteFun a
    }

-------------------------------------------------------------------------------------
-- identity instances

instance (MonadIO m) => ToRoute (Route m) where
  type RouteMonad (Route m) = m
  toRouteInfo = id
  toRouteFun = (.run)

-------------------------------------------------------------------------------------
-- request inputs

-- | Generic case for request body
newtype Body media a = Body a

instance (ToSchema a, FromReqBody media a, ToRoute b) => ToRoute (Body media a -> b) where
  type RouteMonad (Body media a -> b) = RouteMonad b

  toRouteInfo = addBodyInfo @media @a . toRouteInfo @b
  toRouteFun f = withBody @media (toRouteFun . f . Body)

newtype Query (sym :: Symbol) a = Query a

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Query sym a -> b) where
  type RouteMonad (Query sym a -> b) = RouteMonad b

  toRouteInfo = addQueryInfo @sym @a . toRouteInfo @b
  toRouteFun f = withQuery (getName @sym) (toRouteFun . f . Query)

newtype Optional (sym :: Symbol) a = Optional (Maybe a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Optional sym a -> b) where
  type RouteMonad (Optional sym a -> b) = RouteMonad b

  toRouteInfo = addOptionalInfo @sym @a . toRouteInfo @b
  toRouteFun f = withOptional (getName @sym) (toRouteFun . f . Optional)

newtype QueryFlag (sym :: Symbol) = QueryFlag Bool

instance (ToRoute b, KnownSymbol sym) => ToRoute (QueryFlag sym -> b) where
  type RouteMonad (QueryFlag sym -> b) = RouteMonad b

  toRouteInfo = addQueryFlagInfo @sym . toRouteInfo @b
  toRouteFun f = withQueryFlag (getName @sym) (toRouteFun . f . QueryFlag)

newtype Capture (sym :: Symbol) a = Capture a

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Capture sym a -> b) where
  type RouteMonad (Capture sym a -> b) = RouteMonad b

  toRouteInfo = addCaptureInfo @sym @a . toRouteInfo @b
  toRouteFun f = withCapture (getName @sym) (toRouteFun . f . Capture)

newtype Header (sym :: Symbol) a = Header a

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Header sym a -> b) where
  type RouteMonad (Header sym a -> b) = RouteMonad b

  toRouteInfo = addHeaderInfo @sym @a . toRouteInfo @b
  toRouteFun f = withHeader (getName @sym) (toRouteFun . f . Header)

newtype OptionalHeader (sym :: Symbol) a = OptionalHeader (Maybe a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (OptionalHeader sym a -> b) where
  type RouteMonad (OptionalHeader sym a -> b) = RouteMonad b

  toRouteInfo = addOptionalHeaderInfo @sym @a . toRouteInfo @b
  toRouteFun f = withOptionalHeader (getName @sym) (toRouteFun . f . OptionalHeader)

-- | Reads current path info
newtype PathInfo = PathInfo [Text]

instance (ToRoute b) => ToRoute (PathInfo -> b) where
  type RouteMonad (PathInfo -> b) = RouteMonad b
  toRouteInfo = toRouteInfo @b
  toRouteFun f = withPathInfo (toRouteFun . f . PathInfo)

-- | Read low-level request. Note that it does not affect the API schema
newtype RawRequest = RawRequest Request

instance (ToRoute b) => ToRoute (RawRequest -> b) where
  type RouteMonad (RawRequest -> b) = RouteMonad b
  toRouteInfo = toRouteInfo @b
  toRouteFun f = \req -> toRouteFun (f (RawRequest req)) req

-- | Is connection secure (made over SSL)
data IsSecure = IsSecure Bool

instance (ToRoute b) => ToRoute (IsSecure -> b) where
  type RouteMonad (IsSecure -> b) = RouteMonad b
  toRouteInfo = toRouteInfo @b
  toRouteFun f = \req -> toRouteFun (f (IsSecure req.isSecure)) req

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

type Get m a = Send GET m a
type Post m a = Send POST m a
type Put m a = Send PUT m a
type Delete m a = Send DELETE m a
type Options m a = Send OPTIONS m a
type Head m a = Send HEAD m a
type Patch m a = Send PATCH m a
type Trace m a = Send TRACE m a

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

newtype Send method m a = Send {unSend :: m a}

instance {-# OVERLAPPABLE #-} (MonadIO m, IsResp a, IsMethod method) => ToRoute (Send method m a) where
  type RouteMonad (Send method m a) = m
  toRouteInfo = setMethod (toMethod @method) (getMedia @a)
  toRouteFun (Send a) = sendResponse $ toResponse <$> a

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
