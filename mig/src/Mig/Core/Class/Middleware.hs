{-| Middlewares are useful to apply certain action to all routes in the server.
For example we can add generic logger or authorization bazed on common query parameter
or field of the body request that contains token of the session.

The downside is that we work on low level of Requesnce/Response as we have rendered
all routes to ServerFun. But thw good part of it is that we can add generic action
to every route.

Let's consider a simple example of adding logger to lall routes:


> logRoutes :: Server IO -> Server IO
> logRoutes = applyMiddleware $ \(PathInfo path) -> prependServerAction $
>    when (path /= ["favicon.ico"] && headMay path /= Just "static") $ do
>      logRoute site (Text.intercalate "/" path)
>
> -- | Logs the route info
> logRoute :: Site -> Text -> IO ()
> logRoute site route = do
>   time <- getCurrentTime
>   site.logInfo $ route <> " page visited at: " <> Text.pack (show time)

Here we use instance of ToMiddleware for `PathInfo` to read full path for any route
and we use this information in the logger.

We have various instances for everything that we can query from the request
and we can use this information to transform the server functions inside the routes.

The instances work in the same manner as route handlers we can use as many arguments as
we wish and we use typed wrappers to query specific part of the request.
Thus we gain type-safety and get convenient interface to request the various parts of request.
-}
module Mig.Core.Class.Middleware (
  -- * class
  ToMiddleware (..),
  Middleware (..),
  MiddlewareFun,
  toMiddleware,
  ($:),
  applyMiddleware,

  -- * specific middlewares
  prependServerAction,
  appendServerAction,
  processResponse,
  whenSecure,
  processNoResponse,
) where

import Control.Monad.IO.Class
import Data.Kind
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.Proxy
import Data.String
import GHC.TypeLits
import Web.HttpApiData

import Mig.Core.Class.MediaType
import Mig.Core.Class.Response
import Mig.Core.Class.Route
import Mig.Core.Server
import Mig.Core.ServerFun
import Mig.Core.Types

type MiddlewareFun m = ServerFun m -> ServerFun m

data Middleware m = Middleware
  { info :: RouteInfo -> RouteInfo
  , run :: MiddlewareFun m
  }

instance Monoid (Middleware m) where
  mempty = Middleware id id

instance Semigroup (Middleware m) where
  (<>) a b = Middleware (a.info . b.info) (a.run . b.run)

-- | Infix operator for applyMiddleware
($:) :: forall f. (ToMiddleware f) => f -> Server (MiddlewareMonad f) -> Server (MiddlewareMonad f)
($:) = applyMiddleware

applyMiddleware :: forall f. (ToMiddleware f) => f -> Server (MiddlewareMonad f) -> Server (MiddlewareMonad f)
applyMiddleware a = mapRouteInfo (toMiddlewareInfo @f) . mapServerFun (toMiddlewareFun a)

class (MonadIO (MiddlewareMonad f)) => ToMiddleware f where
  type MiddlewareMonad f :: Type -> Type
  toMiddlewareInfo :: RouteInfo -> RouteInfo
  toMiddlewareFun :: f -> ServerFun (MiddlewareMonad f) -> ServerFun (MiddlewareMonad f)

toMiddleware :: forall f. (ToMiddleware f) => f -> Middleware (MiddlewareMonad f)
toMiddleware a = Middleware (toMiddlewareInfo @f) (toMiddlewareFun a)

-- identity
instance (MonadIO m) => ToMiddleware (MiddlewareFun m) where
  type MiddlewareMonad (ServerFun m -> ServerFun m) = m
  toMiddlewareInfo = id
  toMiddlewareFun = id

instance (MonadIO m) => ToMiddleware (Middleware m) where
  type MiddlewareMonad (Middleware m) = m
  toMiddlewareInfo = id
  toMiddlewareFun = (.run)

-- path info
instance (ToMiddleware a) => ToMiddleware (PathInfo -> a) where
  type MiddlewareMonad (PathInfo -> a) = MiddlewareMonad a
  toMiddlewareInfo = id
  toMiddlewareFun f = \fun -> withPathInfo (\path -> toMiddlewareFun (f (PathInfo path)) fun)

-- path info
instance (ToMiddleware a) => ToMiddleware (IsSecure -> a) where
  type MiddlewareMonad (IsSecure -> a) = MiddlewareMonad a
  toMiddlewareInfo = id
  toMiddlewareFun f = \fun -> \req -> (toMiddlewareFun (f (IsSecure req.isSecure)) fun) req

instance (ToMiddleware a) => ToMiddleware (RawRequest -> a) where
  type MiddlewareMonad (RawRequest -> a) = MiddlewareMonad a
  toMiddlewareInfo = id
  toMiddlewareFun f = \fun -> \req -> (toMiddlewareFun (f (RawRequest req)) fun) req

-- request body
instance (FromReqBody ty a, ToSchema a, ToMiddleware b) => ToMiddleware (Body ty a -> b) where
  type MiddlewareMonad (Body ty a -> b) = MiddlewareMonad b
  toMiddlewareInfo = addBodyInfo @ty @a . toMiddlewareInfo @b
  toMiddlewareFun f = \fun -> withBody @ty (\body -> toMiddlewareFun (f (Body body)) fun)

-- header
instance (FromHttpApiData a, ToParamSchema a, ToMiddleware b, KnownSymbol sym) => ToMiddleware (Header sym a -> b) where
  type MiddlewareMonad (Header sym a -> b) = MiddlewareMonad b
  toMiddlewareInfo = addHeaderInfo @sym @a . toMiddlewareInfo @b
  toMiddlewareFun f = \fun -> withHeader (getName @sym) (\a -> toMiddlewareFun (f (Header a)) fun)

-- optional header
instance (FromHttpApiData a, ToParamSchema a, ToMiddleware b, KnownSymbol sym) => ToMiddleware (OptionalHeader sym a -> b) where
  type MiddlewareMonad (OptionalHeader sym a -> b) = MiddlewareMonad b
  toMiddlewareInfo = addOptionalHeaderInfo @sym @a . toMiddlewareInfo @b
  toMiddlewareFun f = \fun -> withOptionalHeader (getName @sym) (\a -> toMiddlewareFun (f (OptionalHeader a)) fun)

-- query
instance (FromHttpApiData a, ToParamSchema a, ToMiddleware b, KnownSymbol sym) => ToMiddleware (Query sym a -> b) where
  type MiddlewareMonad (Query sym a -> b) = MiddlewareMonad b
  toMiddlewareInfo = addQueryInfo @sym @a . toMiddlewareInfo @b
  toMiddlewareFun f = \fun -> withQuery (getName @sym) (\a -> toMiddlewareFun (f (Query a)) fun)

-- optional query
instance (FromHttpApiData a, ToParamSchema a, ToMiddleware b, KnownSymbol sym) => ToMiddleware (Optional sym a -> b) where
  type MiddlewareMonad (Optional sym a -> b) = MiddlewareMonad b
  toMiddlewareInfo = addOptionalInfo @sym @a . toMiddlewareInfo @b
  toMiddlewareFun f = \fun -> withOptional (getName @sym) (\a -> toMiddlewareFun (f (Optional a)) fun)

-- capture
instance (FromHttpApiData a, ToParamSchema a, ToMiddleware b, KnownSymbol sym) => ToMiddleware (Capture sym a -> b) where
  type MiddlewareMonad (Capture sym a -> b) = MiddlewareMonad b
  toMiddlewareInfo = addCaptureInfo @sym @a . toMiddlewareInfo @b
  toMiddlewareFun f = \fun -> withCapture (getName @sym) (\a -> toMiddlewareFun (f (Capture a)) fun)

-- query flag
instance (ToMiddleware b, KnownSymbol sym) => ToMiddleware (QueryFlag sym -> b) where
  type MiddlewareMonad (QueryFlag sym -> b) = MiddlewareMonad b
  toMiddlewareInfo = addQueryFlagInfo @sym . toMiddlewareInfo @b
  toMiddlewareFun f = \fun -> withQueryFlag (getName @sym) (\a -> toMiddlewareFun (f (QueryFlag a)) fun)

---------------------------------------------
-- specific middlewares

-- | Prepends action to the server
prependServerAction :: forall m. (MonadIO m) => m () -> Middleware m
prependServerAction act = toMiddleware go
  where
    go :: ServerFun m -> ServerFun m
    go f = \req -> do
      act
      f req

-- | Post appends action to the server
appendServerAction :: forall m. (MonadIO m) => m () -> Middleware m
appendServerAction act = toMiddleware go
  where
    go :: ServerFun m -> ServerFun m
    go f = \req -> do
      resp <- f req
      act
      pure resp

-- | Applies transformation to the response
processResponse :: forall m. (MonadIO m) => (m (Maybe Response) -> m (Maybe Response)) -> Middleware m
processResponse act = toMiddleware go
  where
    go :: ServerFun m -> ServerFun m
    go f = \req -> do
      act (f req)

-- | Execute request only if it is secure (made with SSL connection)
whenSecure :: forall m. (MonadIO m) => Middleware m
whenSecure = toMiddleware go
  where
    go :: IsSecure -> MiddlewareFun m
    go (IsSecure isSecure) fun = \req -> do
      if isSecure
        then fun req
        else pure Nothing

-- | Sets default response if server response with Nothing. If it can not handle the request.
processNoResponse :: forall m a. (MonadIO m, IsResp a) => m a -> Middleware m
processNoResponse defaultResponse = toMiddleware go
  where
    go :: MiddlewareFun m
    go fun = \req -> do
      mResp <- fun req
      case mResp of
        Just resp -> pure (Just resp)
        Nothing -> Just . toResponse <$> defaultResponse

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
