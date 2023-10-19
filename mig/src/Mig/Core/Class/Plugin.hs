{-| Plugins are useful to apply certain action to all routes in the server.
For example we can add generic logger or authorization bazed on common query parameter
or field of the body request that contains token of the session.

The downside is that we work on low level of Requesnce/Response as we have rendered
all routes to ServerFun. But thw good part of it is that we can add generic action
to every route.

Let's consider a simple example of adding logger to lall routes:


> logRoutes :: Server IO -> Server IO
> logRoutes = applyPlugin $ \(PathInfo path) -> prependServerAction $
>    when (path /= ["favicon.ico"] && headMay path /= Just "static") $ do
>      logRoute site (Text.intercalate "/" path)
>
> -- | Logs the route info
> logRoute :: Site -> Text -> IO ()
> logRoute site route = do
>   time <- getCurrentTime
>   site.logInfo $ route <> " page visited at: " <> Text.pack (show time)

Here we use instance of ToPlugin for `PathInfo` to read full path for any route
and we use this information in the logger.

We have various instances for everything that we can query from the request
and we can use this information to transform the server functions inside the routes.

The instances work in the same manner as route handlers we can use as many arguments as
we wish and we use typed wrappers to query specific part of the request.
Thus we gain type-safety and get convenient interface to request the various parts of request.
-}
module Mig.Core.Class.Plugin (
  -- * class
  ToPlugin (..),
  Plugin (..),
  PluginFun,
  toPlugin,
  fromPluginFun,
  ($:),
  applyPlugin,
  RawResponse (..),

  -- * specific plugins
  prependServerAction,
  appendServerAction,
  processResponse,
  whenSecure,
  processNoResponse,
) where

import Control.Monad.IO.Class
import Data.OpenApi (ToParamSchema (..), ToSchema (..))
import Data.Proxy
import Data.String
import GHC.TypeLits
import Web.HttpApiData

import Mig.Core.Class.MediaType
import Mig.Core.Class.Monad
import Mig.Core.Class.Response
import Mig.Core.Server
import Mig.Core.ServerFun
import Mig.Core.Types

-- | Low-level plugin function.
type PluginFun m = ServerFun m -> ServerFun m

{-| Plugin can convert all routes of the server.
It is wrapper on top of @ServerFun m -> ServerFun m@.
We can apply plugins to servers with @applyPlugin@ function
also plugin has Monoid instance which is like Monoid.Endo or functional composition @(.)@.
-}
data Plugin m = Plugin
  { info :: RouteInfo -> RouteInfo
  -- ^ update api schema
  , run :: PluginFun m
  -- ^ run the plugin
  }

instance Monoid (Plugin m) where
  mempty = Plugin id id

instance Semigroup (Plugin m) where
  (<>) a b = Plugin (a.info . b.info) (a.run . b.run)

-- | Infix operator for @applyPlugin@
($:) :: forall f. (ToPlugin f) => f -> Server (MonadOf f) -> Server (MonadOf f)
($:) = applyPlugin

-- | Applies plugin to all routes of the server.
applyPlugin :: forall f. (ToPlugin f) => f -> Server (MonadOf f) -> Server (MonadOf f)
applyPlugin a = mapRouteInfo (toPluginInfo @f) . mapServerFun (toPluginFun a)

{-| Values that can represent a plugin.
We use various newtype-wrappers to query type-safe info from request.
-}
class (MonadIO (MonadOf f)) => ToPlugin f where
  toPluginInfo :: RouteInfo -> RouteInfo
  toPluginFun :: f -> ServerFun (MonadOf f) -> ServerFun (MonadOf f)

-- | Convert plugin-like value to plugin.
toPlugin :: forall f. (ToPlugin f) => f -> Plugin (MonadOf f)
toPlugin a = Plugin (toPluginInfo @f) (toPluginFun a)

-- identity
instance (MonadIO m) => ToPlugin (PluginFun m) where
  toPluginInfo = id
  toPluginFun = id

instance (MonadIO m) => ToPlugin (Plugin m) where
  toPluginInfo = id
  toPluginFun = (.run)

fromPluginFun :: (MonadIO m) => PluginFun m -> Plugin m
fromPluginFun = toPlugin

-- path info
instance (ToPlugin a) => ToPlugin (PathInfo -> a) where
  toPluginInfo = id
  toPluginFun f = \fun -> withPathInfo (\path -> toPluginFun (f (PathInfo path)) fun)

-- path info
instance (ToPlugin a) => ToPlugin (IsSecure -> a) where
  toPluginInfo = id
  toPluginFun f = \fun -> \req -> (toPluginFun (f (IsSecure req.isSecure)) fun) req

instance (ToPlugin a) => ToPlugin (RawRequest -> a) where
  toPluginInfo = id
  toPluginFun f = \fun -> \req -> (toPluginFun (f (RawRequest req)) fun) req

-- | Read low-level response. Note that it does not affect the API schema
newtype RawResponse = RawResponse (Maybe Response)

instance (ToPlugin a) => ToPlugin (RawResponse -> a) where
  toPluginInfo = id
  toPluginFun f = \fun -> \req -> do
    resp <- fun req
    (toPluginFun (f (RawResponse resp)) fun) req

-- request body
instance (FromReqBody ty a, ToSchema a, ToPlugin b) => ToPlugin (Body ty a -> b) where
  toPluginInfo = addBodyInfo @ty @a . toPluginInfo @b
  toPluginFun f = \fun -> withBody @ty (\body -> toPluginFun (f (Body body)) fun)

-- header
instance (FromHttpApiData a, ToParamSchema a, ToPlugin b, KnownSymbol sym) => ToPlugin (Header sym a -> b) where
  toPluginInfo = addHeaderInfo @sym @a . toPluginInfo @b
  toPluginFun f = \fun -> withHeader (getName @sym) (\a -> toPluginFun (f (Header a)) fun)

-- optional header
instance (FromHttpApiData a, ToParamSchema a, ToPlugin b, KnownSymbol sym) => ToPlugin (OptionalHeader sym a -> b) where
  toPluginInfo = addOptionalHeaderInfo @sym @a . toPluginInfo @b
  toPluginFun f = \fun -> withOptionalHeader (getName @sym) (\a -> toPluginFun (f (OptionalHeader a)) fun)

-- query
instance (FromHttpApiData a, ToParamSchema a, ToPlugin b, KnownSymbol sym) => ToPlugin (Query sym a -> b) where
  toPluginInfo = addQueryInfo @sym @a . toPluginInfo @b
  toPluginFun f = \fun -> withQuery (getName @sym) (\a -> toPluginFun (f (Query a)) fun)

-- optional query
instance (FromHttpApiData a, ToParamSchema a, ToPlugin b, KnownSymbol sym) => ToPlugin (Optional sym a -> b) where
  toPluginInfo = addOptionalInfo @sym @a . toPluginInfo @b
  toPluginFun f = \fun -> withOptional (getName @sym) (\a -> toPluginFun (f (Optional a)) fun)

-- capture
instance (FromHttpApiData a, ToParamSchema a, ToPlugin b, KnownSymbol sym) => ToPlugin (Capture sym a -> b) where
  toPluginInfo = addCaptureInfo @sym @a . toPluginInfo @b
  toPluginFun f = \fun -> withCapture (getName @sym) (\a -> toPluginFun (f (Capture a)) fun)

-- query flag
instance (ToPlugin b, KnownSymbol sym) => ToPlugin (QueryFlag sym -> b) where
  toPluginInfo = addQueryFlagInfo @sym . toPluginInfo @b
  toPluginFun f = \fun -> withQueryFlag (getName @sym) (\a -> toPluginFun (f (QueryFlag a)) fun)

---------------------------------------------
-- specific plugins

-- | Prepends action to the server
prependServerAction :: forall m. (MonadIO m) => m () -> Plugin m
prependServerAction act = toPlugin go
  where
    go :: ServerFun m -> ServerFun m
    go f = \req -> do
      act
      f req

-- | Post appends action to the server
appendServerAction :: forall m. (MonadIO m) => m () -> Plugin m
appendServerAction act = toPlugin go
  where
    go :: ServerFun m -> ServerFun m
    go f = \req -> do
      resp <- f req
      act
      pure resp

-- | Applies transformation to the response
processResponse :: forall m. (MonadIO m) => (m (Maybe Response) -> m (Maybe Response)) -> Plugin m
processResponse act = toPlugin go
  where
    go :: ServerFun m -> ServerFun m
    go f = \req -> do
      act (f req)

-- | Execute request only if it is secure (made with SSL connection)
whenSecure :: forall m. (MonadIO m) => Plugin m
whenSecure = toPlugin $ \(IsSecure isSecure) ->
  processResponse (if isSecure then id else const (pure Nothing))

-- | Sets default response if server response with Nothing. If it can not handle the request.
processNoResponse :: forall m a. (MonadIO m, IsResp a) => m a -> Plugin m
processNoResponse defaultResponse = toPlugin go
  where
    go :: PluginFun m
    go fun = \req -> do
      mResp <- fun req
      case mResp of
        Just resp -> pure (Just resp)
        Nothing -> Just . toResponse <$> defaultResponse

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))
