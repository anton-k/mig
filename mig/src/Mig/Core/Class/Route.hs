{-# LANGUAGE UndecidableInstances #-}

-- | Creation of routes from functions
module Mig.Core.Class.Route (
  Route (..),
  ToRoute (..),
  toRoute,
  ServerFun,

  -- * Inputs
  ReqBody (..),
  Query (..),
  QueryFlag (..),
  Optional (..),
  Capture (..),
  Header (..),
  OptionalHeader (..),
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

class (MonadIO (RouteMonad a), ToRouteInfo a) => ToRoute a where
  -- | Underyling server monad
  type RouteMonad a :: Type -> Type

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

instance ToRouteInfo (Route m) where
  toRouteInfo = id

instance (MonadIO m) => ToRoute (Route m) where
  type RouteMonad (Route m) = m
  toRouteFun = (.run)

-------------------------------------------------------------------------------------
-- request inputs

-- | Generic case for request body
newtype ReqBody media a = ReqBody a

instance (ToSchema a, ToMediaType ty, ToRouteInfo b) => ToRouteInfo (ReqBody ty a -> b) where
  toRouteInfo = addRouteInput (ReqBodyInput (toMediaType @ty) (toSchemaDefs @a)) . toRouteInfo @b

instance (ToSchema a, FromReqBody media a, ToRoute b) => ToRoute (ReqBody media a -> b) where
  type RouteMonad (ReqBody media a -> b) = RouteMonad b

  toRouteFun f = withBody @media (toRouteFun . f . ReqBody)

newtype Query (sym :: Symbol) a = Query a

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Query sym a -> b) where
  toRouteInfo = addRouteInput (QueryInput (IsRequired True) (getName @sym) (toParamSchema (Proxy @a))) . toRouteInfo @b

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Query sym a -> b) where
  type RouteMonad (Query sym a -> b) = RouteMonad b

  toRouteFun f = withQuery (getName @sym) (toRouteFun . f . Query)

newtype Optional (sym :: Symbol) a = Optional (Maybe a)

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Optional sym a -> b) where
  toRouteInfo = addRouteInput (QueryInput (IsRequired False) (getName @sym) (toParamSchema (Proxy @a))) . toRouteInfo @b

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Optional sym a -> b) where
  type RouteMonad (Optional sym a -> b) = RouteMonad b

  toRouteFun f = withOptional (getName @sym) (toRouteFun . f . Optional)

newtype QueryFlag (sym :: Symbol) = QueryFlag Bool

instance (KnownSymbol sym, ToRouteInfo b) => ToRouteInfo (QueryFlag sym -> b) where
  toRouteInfo = addRouteInput (QueryFlagInput (getName @sym)) . toRouteInfo @b

instance (ToRoute b, KnownSymbol sym) => ToRoute (QueryFlag sym -> b) where
  type RouteMonad (QueryFlag sym -> b) = RouteMonad b

  toRouteFun f = withQueryFlag (getName @sym) (toRouteFun . f . QueryFlag)

newtype Capture (sym :: Symbol) a = Capture a

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Capture sym a -> b) where
  toRouteInfo = addRouteInput (CaptureInput (getName @sym) (toParamSchema (Proxy @a))) . toRouteInfo @b

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Capture sym a -> b) where
  type RouteMonad (Capture sym a -> b) = RouteMonad b

  toRouteFun f = withCapture (getName @sym) (toRouteFun . f . Capture)

newtype Header (sym :: Symbol) a = Header a

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (Header sym a -> b) where
  toRouteInfo = addRouteInput (HeaderInput (IsRequired True) (getName @sym) (toParamSchema (Proxy @a))) . toRouteInfo @b

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (Header sym a -> b) where
  type RouteMonad (Header sym a -> b) = RouteMonad b

  toRouteFun f = withHeader (getName @sym) (toRouteFun . f . Header)

newtype OptionalHeader (sym :: Symbol) a = OptionalHeader (Maybe a)

instance (KnownSymbol sym, ToParamSchema a, ToRouteInfo b) => ToRouteInfo (OptionalHeader sym a -> b) where
  toRouteInfo = addRouteInput (HeaderInput (IsRequired False) (getName @sym) (toParamSchema (Proxy @a))) . toRouteInfo @b

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToRoute (OptionalHeader sym a -> b) where
  type RouteMonad (OptionalHeader sym a -> b) = RouteMonad b

  toRouteFun f = withOptionalHeader (getName @sym) (toRouteFun . f . OptionalHeader)

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

instance {-# OVERLAPPABLE #-} (IsMethod method, IsResp a) => ToRouteInfo (Send method m a) where
  toRouteInfo = setMethod (toMethod @method) (getMedia @a)

instance {-# OVERLAPPABLE #-} (MonadIO m, IsResp a, IsMethod method) => ToRoute (Send method m a) where
  type RouteMonad (Send method m a) = m
  toRouteFun (Send a) = sendResponse $ toResponse <$> a

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
