module Mig.Server.Class (
  HasServer (..),
  hoistServer,
  fromReader,
  fromReaderExcept,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Kind
import Data.Text (Text)

import Mig.Core.Class
import Mig.Core.Server
import Mig.Core.Types

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
