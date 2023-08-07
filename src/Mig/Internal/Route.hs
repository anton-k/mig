-- | Creation of routes from functions
module Mig.Internal.Route
  ( Route (..)
  , ToRoute (..)
  , toRoute
  , ServerFun (..)

  -- * Inputs
  , Body (..)
  , RawBody (..)
  , Query (..)
  , Optional (..)
  , Capture (..)
  , Header (..)
  , FormBody (..)
  , PathInfo (..)

  -- * Outputs
  , SetStatus (..)
  , AddHeaders (..)

  -- * Output methods
  , Get (..)
  , Post (..)
  , Put (..)
  ) where

import Data.Text (Text)
import Mig.Internal.Info
import Mig.Internal.ServerFun
import Mig.Internal.Types (ToTextResp (..), ToJsonResp (..), ToHtmlResp (..), setRespStatus)
import Data.Kind
import Data.Aeson (FromJSON)
import Web.HttpApiData
import Web.FormUrlEncoded
import GHC.TypeLits
import Data.Proxy
import Data.String
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Method
import Text.Blaze.Html (Html)
import Mig.Internal.Types qualified as Resp (Resp (..))

class ToRouteInfo a => ToRoute a where
  -- | Underyling server monad
  type  RouteMonad a :: Type -> Type

  -- | Convert to route
  toRouteFun :: a -> ServerFun (RouteMonad a)

  -- | route that produce nothing
  emptyRoute :: a

-- | Route contains API-info and how to run it
data Route m = Route
  { api :: RouteInfo
  , run :: ServerFun m
  }

toRoute :: forall a . ToRoute a => a -> Route (RouteMonad a)
toRoute a = Route
  { api = toRouteInfo @a emptyRouteInfo
  , run = toRouteFun a
  }

-------------------------------------------------------------------------------------
-- identity instances

instance ToRouteInfo (Route m) where
  toRouteInfo = id

instance Monad m => ToRoute (Route m) where
  type RouteMonad (Route m) = m
  toRouteFun = (.run)
  emptyRoute = Route emptyRouteInfo emptyRoute

instance Monad m => ToRoute (ServerFun m) where
  type RouteMonad (ServerFun m) = m
  toRouteFun = id
  emptyRoute = ServerFun $ const $ pure Nothing

-------------------------------------------------------------------------------------
-- request inputs

newtype Body a = Body a

instance (ToJsonSpec a, ToRouteInfo b) => ToRouteInfo (Body a -> b) where
  toRouteInfo = addRouteInput (BodyJsonInput (toJsonSpec @a)) . toRouteInfo @b

instance (ToJsonSpec a, FromJSON a, ToRoute b) => ToRoute (Body a -> b) where
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

instance (KnownSymbol sym, ToPrimType a, ToRouteInfo b) => ToRouteInfo (Query sym a -> b) where
  toRouteInfo = addRouteInput (QueryInput name (toPrimType @a)) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToPrimType a, ToRoute b, KnownSymbol sym) => ToRoute (Query sym a -> b) where
  type RouteMonad (Query sym a -> b) = RouteMonad b

  toRouteFun f = withQuery name (toRouteFun . f . Query)
    where
      name = fromString (symbolVal (Proxy @sym))

  emptyRoute = const emptyRoute

newtype Optional (sym :: Symbol) a = Optional (Maybe a)

instance (KnownSymbol sym, ToPrimType a, ToRouteInfo b) => ToRouteInfo (Optional sym a -> b) where
  toRouteInfo = addRouteInput (QueryInput name (toPrimType @a)) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToPrimType a, ToRoute b, KnownSymbol sym) => ToRoute (Optional sym a -> b) where
  type RouteMonad (Optional sym a -> b) = RouteMonad b

  toRouteFun f = withOptional name (toRouteFun . f . Optional)
    where
      name = fromString (symbolVal (Proxy @sym))

  emptyRoute = const emptyRoute

newtype Capture (sym :: Symbol) a = Capture a

instance (KnownSymbol sym, ToPrimType a, ToRouteInfo b) => ToRouteInfo (Capture sym a -> b) where
  toRouteInfo = addRouteInput (CaptureInput name (toPrimType @a)) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToPrimType a, ToRoute b, KnownSymbol sym) => ToRoute (Capture sym a -> b) where
  type RouteMonad (Capture sym a -> b) = RouteMonad b

  toRouteFun f = withCapture name (toRouteFun . f . Capture)
    where
      name = fromString (symbolVal (Proxy @sym))

  emptyRoute = const emptyRoute

newtype Header (sym :: Symbol) a = Header a

instance (KnownSymbol sym, ToPrimType a, ToRouteInfo b) => ToRouteInfo (Header sym a -> b) where
  toRouteInfo = addRouteInput (HeaderInput name (toPrimType @a)) . toRouteInfo @b
    where
      name = fromString (symbolVal (Proxy @sym))

instance (FromHttpApiData a, ToPrimType a, ToRoute b, KnownSymbol sym) => ToRoute (Header sym a -> b) where
  type RouteMonad (Header sym a -> b) = RouteMonad b

  toRouteFun f = withHeader name (toRouteFun . f . Header)
    where
      name = fromString (symbolVal (Proxy @sym))

  emptyRoute = const emptyRoute

newtype FormBody a = FormBody a

instance (ToFormType a, ToRouteInfo b) => ToRouteInfo (FormBody a -> b) where
  toRouteInfo = addRouteInput (FormBodyInput (toFormType @a)) . toRouteInfo @b

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

-- Get method

-- | Get method. Note that we can not use body input with Get-method, use Post for that.
-- So with Get we can use only URI inputs (Query, Optional, Capture)
newtype Get ty m a = Get (m a)

instance (ToMediaType ty) => ToRouteInfo (Get ty m a) where
  toRouteInfo = setMethod methodGet (toMediaType @ty)

instance (Monad m, ToTextResp a) => ToRoute (Get Text m a) where
  type RouteMonad (Get Text m a) = m
  toRouteFun (Get a) = sendText a
  emptyRoute = Get (pure undefined)

instance (Monad m, ToJsonResp a) => ToRoute (Get Json m a) where
  type RouteMonad (Get Json m a) = m
  toRouteFun (Get a) = sendJson a
  emptyRoute = Get (pure undefined)

instance (Monad m, ToHtmlResp a) => ToRoute (Get Html m a) where
  type RouteMonad (Get Html m a) = m
  toRouteFun (Get a) = sendHtml a
  emptyRoute = Get (pure undefined)

instance (Monad m) => ToRoute (Get BL.ByteString m BL.ByteString) where
  type RouteMonad (Get BL.ByteString m BL.ByteString) = m
  toRouteFun (Get a) = sendRaw a
  emptyRoute = Get (pure undefined)

-- Post method

-- | Post method.

newtype Post ty m a = Post (m a)

instance (ToMediaType ty) => ToRouteInfo (Post ty m a) where
  toRouteInfo = setMethod methodPost (toMediaType @ty)

instance (Monad m, ToTextResp a) => ToRoute (Post Text m a) where
  type RouteMonad (Post Text m a) = m
  toRouteFun (Post a) = sendText a
  emptyRoute = Post (pure undefined)

instance (Monad m, ToJsonResp a) => ToRoute (Post Json m a) where
  type RouteMonad (Post Json m a) = m
  toRouteFun (Post a) = sendJson a
  emptyRoute = Post (pure undefined)

instance (Monad m, ToHtmlResp a) => ToRoute (Post Html m a) where
  type RouteMonad (Post Html m a) = m
  toRouteFun (Post a) = sendHtml a
  emptyRoute = Post (pure undefined)

instance (Monad m) => ToRoute (Post BL.ByteString m BL.ByteString) where
  type RouteMonad (Post BL.ByteString m BL.ByteString) = m
  toRouteFun (Post a) = sendRaw a
  emptyRoute = Post (pure undefined)

-- Put method

-- | Put method.

newtype Put ty m a = Put (m a)

instance (ToMediaType ty) => ToRouteInfo (Put ty m a) where
  toRouteInfo = setMethod methodPost (toMediaType @ty)

instance (Monad m, ToTextResp a) => ToRoute (Put Text m a) where
  type RouteMonad (Put Text m a) = m
  toRouteFun (Put a) = sendText a
  emptyRoute = Put (pure undefined)

instance (Monad m, ToJsonResp a) => ToRoute (Put Json m a) where
  type RouteMonad (Put Json m a) = m
  toRouteFun (Put a) = sendJson a
  emptyRoute = Put (pure undefined)

instance (Monad m, ToHtmlResp a) => ToRoute (Put Html m a) where
  type RouteMonad (Put Html m a) = m
  toRouteFun (Put a) = sendHtml a
  emptyRoute = Put (pure undefined)

instance (Monad m) => ToRoute (Put BL.ByteString m BL.ByteString) where
  type RouteMonad (Put BL.ByteString m BL.ByteString) = m
  toRouteFun (Put a) = sendRaw a
  emptyRoute = Put (pure undefined)

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

instance ToTextResp a => ToTextResp (AddHeaders a) where
  toTextResp (AddHeaders headers content) =
    resp { Resp.headers = resp.headers <> headers }
    where
      resp = toTextResp content

instance ToTextResp a => ToTextResp (SetStatus a) where
  toTextResp (SetStatus st content) =
    setRespStatus st (toTextResp content)

-- json response

instance ToJsonResp a => ToJsonResp (AddHeaders a) where
  toJsonResp (AddHeaders headers content) =
    resp { Resp.headers = resp.headers <> headers }
    where
      resp = toJsonResp content

instance ToJsonResp a => ToJsonResp (SetStatus a) where
  toJsonResp (SetStatus st content) =
    setRespStatus st (toJsonResp content)

-- html response

instance ToHtmlResp a => ToHtmlResp (AddHeaders a) where
  toHtmlResp (AddHeaders headers content) =
    resp { Resp.headers = resp.headers <> headers }
    where
      resp = toHtmlResp content

instance ToHtmlResp a => ToHtmlResp (SetStatus a) where
  toHtmlResp (SetStatus st content) =
    setRespStatus st (toHtmlResp content)
