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

{-| Values that represent routes.
A route is a function of arbitrary number of arguments. Where
each argument is one of the special newtype-wrappers that
read type-safe information from HTTP-request and return type of the route function
is a value of something convertible to HTTP-request.
-}
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

-- | converts route-like value to route.
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

-- | Generic case for request body. The type encodes a media type and value of the request body.
newtype Body media a = Body a

instance (ToSchema a, FromReqBody media a, ToRoute b) => ToRoute (Body media a -> b) where
  type RouteMonad (Body media a -> b) = RouteMonad b

  toRouteInfo = addBodyInfo @media @a . toRouteInfo @b
  toRouteFun f = withBody @media (toRouteFun . f . Body)

{-| Required URL parameter query.

> "api/route?foo=bar" ==> (Query bar) :: Query "foo" a
-}
newtype Query (sym :: Symbol) a = Query a

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Query sym a -> b) where
  type RouteMonad (Query sym a -> b) = RouteMonad b

  toRouteInfo = addQueryInfo @sym @a . toRouteInfo @b
  toRouteFun f = withQuery (getName @sym) (toRouteFun . f . Query)

{-| Optional URL parameter query.

> "api/route?foo=bar" ==> (Optional maybeBar) :: Query "foo" a
-}
newtype Optional (sym :: Symbol) a = Optional (Maybe a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Optional sym a -> b) where
  type RouteMonad (Optional sym a -> b) = RouteMonad b

  toRouteInfo = addOptionalInfo @sym @a . toRouteInfo @b
  toRouteFun f = withOptional (getName @sym) (toRouteFun . f . Optional)

{-| Query flag. It is a boolean value in the URL-query. If it is missing
it is @False@ if it is in the query but does not have any value it is @True@.
Also it can have values @true/false@ in the query.
-}
newtype QueryFlag (sym :: Symbol) = QueryFlag Bool

instance (ToRoute b, KnownSymbol sym) => ToRoute (QueryFlag sym -> b) where
  type RouteMonad (QueryFlag sym -> b) = RouteMonad b

  toRouteInfo = addQueryFlagInfo @sym . toRouteInfo @b
  toRouteFun f = withQueryFlag (getName @sym) (toRouteFun . f . QueryFlag)

{-| Argument of capture from the query.

> "api/route/{foo} if api/route/bar passed"  ==> (Capture bar) :: Capture "Foo" barType
-}
newtype Capture (sym :: Symbol) a = Capture a

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Capture sym a -> b) where
  type RouteMonad (Capture sym a -> b) = RouteMonad b

  toRouteInfo = addCaptureInfo @sym @a . toRouteInfo @b
  toRouteFun f = withCapture (getName @sym) (toRouteFun . f . Capture)

{-| Reads value from the required header by name. For example if the request has header:

> "foo": "bar"

It reads the value:

> (Header bar) :: Header "foo" barType
-}
newtype Header (sym :: Symbol) a = Header a

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Header sym a -> b) where
  type RouteMonad (Header sym a -> b) = RouteMonad b

  toRouteInfo = addHeaderInfo @sym @a . toRouteInfo @b
  toRouteFun f = withHeader (getName @sym) (toRouteFun . f . Header)

{-| Reads value from the optional header by name. For example if the request has header:

> "foo": "bar"

It reads the value:

> (OptionalHeader (Just bar)) :: OptionalHeader "foo" barType
-}
newtype OptionalHeader (sym :: Symbol) a = OptionalHeader (Maybe a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (OptionalHeader sym a -> b) where
  type RouteMonad (OptionalHeader sym a -> b) = RouteMonad b

  toRouteInfo = addOptionalHeaderInfo @sym @a . toRouteInfo @b
  toRouteFun f = withOptionalHeader (getName @sym) (toRouteFun . f . OptionalHeader)

{-| Reads current path info.

> "api/foo/bar" ==> PathInfo ["foo", "bar"]
-}
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

-- | Reads info on weather the connection is secure (made over SSL).
data IsSecure = IsSecure Bool

instance (ToRoute b) => ToRoute (IsSecure -> b) where
  type RouteMonad (IsSecure -> b) = RouteMonad b
  toRouteInfo = toRouteInfo @b
  toRouteFun f = \req -> toRouteFun (f (IsSecure req.isSecure)) req

-------------------------------------------------------------------------------------
-- outputs

-- | type-level GET-method tag
data GET

-- | type-level POST-method tag
data POST

-- | type-level PUT-method tag
data PUT

-- | type-level DELETE-method tag
data DELETE

-- | type-level OPTIONS-method tag
data OPTIONS

-- | type-level HEAD-method tag
data HEAD

-- | type-level PATCH-method tag
data PATCH

-- | type-level TRACE-method tag
data TRACE

-- | Get request
type Get m a = Send GET m a

-- | Post request
type Post m a = Send POST m a

-- | Put request
type Put m a = Send PUT m a

-- | Delete request
type Delete m a = Send DELETE m a

-- | Options request
type Options m a = Send OPTIONS m a

-- | Head request
type Head m a = Send HEAD m a

-- | Path request
type Patch m a = Send PATCH m a

-- | trace request
type Trace m a = Send TRACE m a

-- | Converts type-level tag for methods to value
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

{-| Route response type. It encodes the route method in the type
and which monad is used and which type the response has.

The repsonse value is usually one of two cases:

* @Resp media a@ -- for routes which always produce a value

* @RespOr media err a@ - for routes that can also produce an error or value.

See the class @IsResp@ for more details on response types.
-}
newtype Send method m a = Send {unSend :: m a}

instance {-# OVERLAPPABLE #-} (MonadIO m, IsResp a, IsMethod method) => ToRoute (Send method m a) where
  type RouteMonad (Send method m a) = m
  toRouteInfo = setMethod (toMethod @method) (getMedia @a)
  toRouteFun (Send a) = sendResponse $ toResponse <$> a

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
