{-# LANGUAGE UndecidableInstances #-}

-- | Creation of routes from functions
module Mig.Core.Class.Route (
  Route (..),
  ToRoute (..),
  toRoute,
) where

import Control.Monad.IO.Class
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.Proxy
import Data.String
import GHC.TypeLits
import Mig.Core.Class.MediaType
import Mig.Core.Class.Monad
import Mig.Core.Class.Response (IsResp (..))
import Mig.Core.ServerFun
import Mig.Core.Types
import Web.HttpApiData

{-| Values that represent routes.
A route is a function of arbitrary number of arguments. Where
each argument is one of the special newtype-wrappers that
read type-safe information from HTTP-request and return type of the route function
is a value of something convertible to HTTP-request.
-}
class (MonadIO (MonadOf a)) => ToRoute a where
  -- | Update API info
  toRouteInfo :: RouteInfo -> RouteInfo

  -- | Convert to route
  toRouteFun :: a -> ServerFun (MonadOf a)

-- | Route contains API-info and how to run it
data Route m = Route
  { info :: RouteInfo
  -- ^ definition of the API (to use it in OpenApi or clients)
  , run :: ServerFun m
  -- ^ how to run a server
  }

-- | converts route-like value to route.
toRoute :: forall a. (ToRoute a) => a -> Route (MonadOf a)
toRoute a =
  Route
    { info = toRouteInfo @a emptyRouteInfo
    , run = toRouteFun a
    }

-------------------------------------------------------------------------------------
-- identity instances

instance (MonadIO m) => ToRoute (Route m) where
  toRouteInfo = id
  toRouteFun = (.run)

-------------------------------------------------------------------------------------
-- request inputs

instance (ToSchema a, FromReqBody media a, ToRoute b) => ToRoute (Body media a -> b) where
  toRouteInfo = addBodyInfo @media @a . toRouteInfo @b
  toRouteFun f = withBody @media (toRouteFun . f . Body)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Query sym a -> b) where
  toRouteInfo = addQueryInfo @sym @a . toRouteInfo @b
  toRouteFun f = withQuery (getName @sym) (toRouteFun . f . Query)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Optional sym a -> b) where
  toRouteInfo = addOptionalInfo @sym @a . toRouteInfo @b
  toRouteFun f = withOptional (getName @sym) (toRouteFun . f . Optional)

instance (ToRoute b, KnownSymbol sym) => ToRoute (QueryFlag sym -> b) where
  toRouteInfo = addQueryFlagInfo @sym . toRouteInfo @b
  toRouteFun f = withQueryFlag (getName @sym) (toRouteFun . f . QueryFlag)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Capture sym a -> b) where
  toRouteInfo = addCaptureInfo @sym @a . toRouteInfo @b
  toRouteFun f = withCapture (getName @sym) (toRouteFun . f . Capture)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Header sym a -> b) where
  toRouteInfo = addHeaderInfo @sym @a . toRouteInfo @b
  toRouteFun f = withHeader (getName @sym) (toRouteFun . f . Header)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (OptionalHeader sym a -> b) where
  toRouteInfo = addOptionalHeaderInfo @sym @a . toRouteInfo @b
  toRouteFun f = withOptionalHeader (getName @sym) (toRouteFun . f . OptionalHeader)

instance (ToRoute b) => ToRoute (PathInfo -> b) where
  toRouteInfo = toRouteInfo @b
  toRouteFun f = withPathInfo (toRouteFun . f . PathInfo)

instance (ToRoute b) => ToRoute (RawRequest -> b) where
  toRouteInfo = toRouteInfo @b
  toRouteFun f = \req -> toRouteFun (f (RawRequest req)) req

instance (ToRoute b) => ToRoute (IsSecure -> b) where
  toRouteInfo = toRouteInfo @b
  toRouteFun f = \req -> toRouteFun (f (IsSecure req.isSecure)) req

-------------------------------------------------------------------------------------
-- outputs

instance {-# OVERLAPPABLE #-} (MonadIO m, IsResp a, IsMethod method) => ToRoute (Send method m a) where
  toRouteInfo = setMethod (toMethod @method) (getMedia @a)
  toRouteFun (Send a) = sendResponse $ toResponse <$> a

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
