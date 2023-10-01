{-# LANGUAGE UndecidableInstances #-}

-- | Creation of routes from functions
module Mig.Core.Route (
  Route (..),
  ToRoute (..),
  toRoute,
  ServerFun (..),

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
  EitherResponse,

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
  AnyMedia,
) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.Proxy
import Data.String
import Data.Text (Text)
import GHC.TypeLits
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

-- | Generic case for request body
newtype ReqBody media a = ReqBody a

instance (ToSchema a, ToMediaType ty, ToRouteInfo b) => ToRouteInfo (ReqBody ty a -> b) where
  toRouteInfo = addRouteInput (ReqBodyInput (toMediaType @ty) (toSchemaDefs @a)) . toRouteInfo @b

instance (ToSchema a, MimeUnrender media a, ToRoute b) => ToRoute (ReqBody media a -> b) where
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

type EitherResponse err a = Either (Response err) (Response a)

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m a) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m (Response a)) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (IsMethod method, ToMediaType ty) => ToRouteInfo (Send method ty m (EitherResponse err a)) where
  toRouteInfo = setMethod (toMethod @method) (toMediaType @ty)

instance {-# OVERLAPPABLE #-} (MonadIO m, MimeRender ty a, IsMethod method) => ToRoute (Send method ty m a) where
  type RouteMonad (Send method ty m a) = m
  toRouteFun (Send a) = sendResp $ ok @ty <$> a

instance {-# OVERLAPPABLE #-} (MonadIO m, MimeRender ty a, IsMethod method) => ToRoute (Send method ty m (Response a)) where
  type RouteMonad (Send method ty m (Response a)) = m
  toRouteFun (Send a) = sendResp $ (\resp -> Resp resp.status (resp.headers <> setContent media) (RawResp media $ mimeRender @ty resp.body)) <$> a
    where
      media = toMediaType @ty

instance {-# OVERLAPPABLE #-} (MonadIO m, MimeRender ty err, MimeRender ty a, IsMethod method) => ToRoute (Send method ty m (EitherResponse err a)) where
  type RouteMonad (Send method ty m (Either (Response err) (Response a))) = m
  toRouteFun (Send a) =
    sendResp $
      ( \eResp -> case eResp of
          Right resp -> Resp resp.status (resp.headers <> setContent media) (RawResp media $ mimeRender @ty resp.body)
          Left err -> Resp err.status (err.headers <> setContent media) (RawResp media $ mimeRender @ty err.body)
      )
        <$> a
    where
      media = toMediaType @ty

---------------------------------------------
-- any media

{-| In case of any media we do not set output media in the Api info
and do not add ContentType to response
-}
data AnyMedia

instance {-# OVERLAPPABLE #-} (IsMethod method) => ToRouteInfo (Send method AnyMedia m (Response BL.ByteString)) where
  toRouteInfo = setMethod (toMethod @method) "*/*"

instance (MonadIO m, IsMethod method) => ToRoute (Send method AnyMedia m (Response BL.ByteString)) where
  type RouteMonad (Send method AnyMedia m (Response BL.ByteString)) = m
  toRouteFun (Send a) = sendResp $ (\resp -> Resp resp.status (resp.headers) (RawResp "*/*" resp.body)) <$> a

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
