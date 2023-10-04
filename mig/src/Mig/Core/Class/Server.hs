-- | To server class
module Mig.Core.Class.Server (
  (/.),
  ToServer (..),
  HasServer (..),
  hoistServer,
  fromReader,
  fromReaderExcept,
  handleRespError,
) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Kind
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Text (Text)
import GHC.TypeLits
import Mig.Core.Api (Api)
import Mig.Core.Api qualified as Api
import Mig.Core.Class.MediaType (FromReqBody (..))
import Mig.Core.Class.Response (IsResp)
import Mig.Core.Class.Route
import Mig.Core.Server (Server (..), mapServerFun)
import Mig.Core.Types.Http (Response, badRequest)
import Web.HttpApiData

infixr 4 /.

(/.) :: (ToServer a) => Api.Path -> a -> Server (ServerMonad a)
(/.) path api
  | null path.unPath = toServer api
  | otherwise =
      case unServer (toServer api) of
        Api.WithPath rest a -> go rest a
        other -> go mempty other
  where
    go rest a = Server $ Api.WithPath (path <> rest) a

class ToServer a where
  type ServerMonad a :: Type -> Type
  toServer :: a -> Server (ServerMonad a)

-- identity

instance ToServer (Api (Route m)) where
  type ServerMonad (Api (Route m)) = m
  toServer = Server

instance ToServer (Server m) where
  type ServerMonad (Server m) = m
  toServer = id

-- outputs
instance (MonadIO m, IsResp a, IsMethod method) => ToServer (Send method m a) where
  type ServerMonad (Send method m a) = m
  toServer a = Server $ Api.HandleRoute (toRoute a)

-- inputs

instance (ToSchema a, FromReqBody media a, ToRoute b) => ToServer (Body media a -> b) where
  type ServerMonad (Body media a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Query sym a -> b) where
  type ServerMonad (Query sym a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Optional sym a -> b) where
  type ServerMonad (Optional sym a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Capture sym a -> b) where
  type ServerMonad (Capture sym a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (FromHttpApiData a, ToParamSchema a, ToRoute b, KnownSymbol sym) => ToServer (Header sym a -> b) where
  type ServerMonad (Header sym a -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

instance (ToRoute b) => ToServer (PathInfo -> b) where
  type ServerMonad (PathInfo -> b) = RouteMonad b
  toServer a = Server $ Api.HandleRoute (toRoute a)

-------------------------------------------------------------------------------------

-- | Map internal monad of the server
hoistServer :: (forall a. m a -> n a) -> Server m -> Server n
hoistServer f (Server server) =
  Server $ fmap (\x -> Route x.info (f . x.run)) server

{-| Class contains types which can be converted to IO-based server to run as with WAI-interface.

We can run plain IO-servers and ReaderT over IO based servers. Readers can be wrapped in newtypes.
In that case we can derive automatically @HasServer@ instance.
-}
class (Monad m) => HasServer m where
  type ServerResult m :: Type
  renderServer :: Server m -> ServerResult m

instance HasServer IO where
  type ServerResult IO = Server IO
  renderServer = id

instance HasServer (ReaderT env IO) where
  type ServerResult (ReaderT env IO) = env -> IO (Server IO)
  renderServer server initEnv = fromReader initEnv server

-- | Render reader server to IO-based server
fromReader :: env -> Server (ReaderT env IO) -> IO (Server IO)
fromReader env server =
  flip runReaderT env $ ReaderT $ \e -> pure $ hoistServer (flip runReaderT e) server

instance HasServer (ReaderT env (ExceptT Text IO)) where
  type
    ServerResult (ReaderT env (ExceptT Text IO)) =
      env -> IO (Server IO)

  renderServer server initEnv = fromReaderExcept initEnv server

fromReaderExcept ::
  forall env.
  env ->
  Server (ReaderT env (ExceptT Text IO)) ->
  IO (Server IO)
fromReaderExcept env server =
  flip runReaderT env $
    ReaderT $
      \e -> pure $ mapServerFun (handle e) server
  where
    handle :: env -> ServerFun (ReaderT env (ExceptT Text IO)) -> ServerFun IO
    handle e f = \req ->
      handleError <$> runExceptT (runReaderT (f req) e)

    handleError :: Either Text (Maybe Response) -> Maybe Response
    handleError = \case
      Right mResp -> mResp
      Left err -> Just $ badRequest @Text err

handleRespError ::
  forall a m.
  (MonadCatch m, Exception a) =>
  (a -> m (Maybe Response)) ->
  Server m ->
  Server m
handleRespError handle = mapServerFun $ \f -> \req -> do
  eResult <- try @m @a (f req)
  case eResult of
    Right res -> pure res
    Left err -> handle err
