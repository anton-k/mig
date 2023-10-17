{-# LANGUAGE UndecidableInstances #-}

-- | To server class
module Mig.Core.Class.Server (
  (/.),
  ToServer (..),
  HasServer (..),
  hoistServer,
  fromReader,
  fromReaderExcept,
) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader
import Data.Kind
import Data.Text (Text)
import Mig.Core.Api qualified as Api
import Mig.Core.Class.Monad
import Mig.Core.Class.Route
import Mig.Core.Server (Server (..), mapServerFun)
import Mig.Core.ServerFun (ServerFun)
import Mig.Core.Types

infixr 4 /.

{-| Constructs server which can handle given path. Example:

> "api/v1/get/info" /. handleInfo

For captures we use wild-cards:

> "api/v1/get/info/*" /. handleInfo

And handle info has capture argument:

> handleInfo :: Capture "nameA" -> Get IO (Resp Json value)

The name for the capture is derived from the type signature of the route handler.
Note that if capture is in the last position of the path we can omit wild cards.
The proper amount of captures will be derived from the type signature of the handler.
-}
(/.) :: (ToServer a) => Api.Path -> a -> Server (MonadOf a)
(/.) path api
  | null path.unPath = toServer api
  | otherwise =
      case unServer (toServer api) of
        Api.WithPath rest a -> go rest a
        other -> go mempty other
  where
    go rest a = Server $ Api.WithPath (path <> rest) a

-- | Values that can be converted to server
class ToServer a where
  -- | Convert value to server
  toServer :: a -> Server (MonadOf a)

-- identity

instance ToServer (Server m) where
  toServer = id

-- list
instance (ToServer a) => ToServer [a] where
  toServer = foldMap toServer

-- routes
instance {-# OVERLAPPABLE #-} (ToRoute a) => ToServer a where
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
  type ServerResult (ReaderT env IO) = env -> Server IO
  renderServer server initEnv = fromReader initEnv server

-- | Render reader server to IO-based server
fromReader :: env -> Server (ReaderT env IO) -> Server IO
fromReader env = hoistServer (flip runReaderT env)

instance HasServer (ReaderT env (ExceptT Text IO)) where
  type
    ServerResult (ReaderT env (ExceptT Text IO)) =
      env -> Server IO

  renderServer server initEnv = fromReaderExcept initEnv server

-- | Render reader with expetT server to IO-based server
fromReaderExcept ::
  forall env.
  env ->
  Server (ReaderT env (ExceptT Text IO)) ->
  Server IO
fromReaderExcept env = mapServerFun (handle env)
  where
    handle :: env -> ServerFun (ReaderT env (ExceptT Text IO)) -> ServerFun IO
    handle e f = \req ->
      handleError <$> runExceptT (runReaderT (f req) e)

    handleError :: Either Text (Maybe Response) -> Maybe Response
    handleError = \case
      Right mResp -> mResp
      Left err -> Just $ badRequest @Text err
