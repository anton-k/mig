{-# Language UndecidableInstances, OverloadedStrings, DerivingStrategies, OverloadedRecordDot #-}
--  | Example shows how to use custom monad with server.
-- We use Json response server with our Monad App which is a newtype over ReaderT-IO.
-- As we can run only @Server IO@ we need to convert the @Server App@ to IO-based server.
-- We can do it with @renderServer@ function from the class @HasServer@.
--
-- We need to derive the class @HasServer@ for our monad App. It can be done easily with @DerivingStrategies@
-- extension as our monad is a newtype wrapper over ReaderT-IO and for that monad server can be derived.
--
-- Also we can derive instance for the newtypes over @ReaderT env (ExceptT err IO)@
module Example.Counter
  ( main
  ) where

-- import Json based server
import Mig.Json
import Control.Monad.Reader
import Data.IORef

main :: IO ()
main = runServer 8085 =<< counter

-- | render server to IO-based one. We can run only IO-based servers
counter :: IO (Server IO)
counter =
  renderServer server =<< initEnv

-------------------------------------------------------------------------------------
-- server types

-- | Custom type for application monad which is based on Reader-IO pattern.
-- Note the HasServer instance. It allows us to render server to IO-based one
-- which we can run as warp + WAI server
newtype App a = App (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, HasServer)

-- | Common shared state
-- We can put more shared state if we need. Like logger state or some interfaces.
data Env = Env
  { current :: IORef Int
  }

-- | Init shared state
initEnv :: IO Env
initEnv = Env <$> newIORef 0

-------------------------------------------------------------------------------------
-- server definition

-- | Server has two routes:
--
-- * get - to querry current state
-- * put - to add some integer to the state
server :: Server App
server =
  "counter" /. "api" /.
    mconcat
      [ "get" /. handleGet
      , "put" /. handlePut
      ]

-- | Get handler. It logs the call and returns current state
handleGet :: Get App Int
handleGet = Get $ do
  logInfo "Call get"
  ref <- asks (.current)
  liftIO $ readIORef ref

-- | Put handler. It logs the call and updates the state with integer which is read from URL
handlePut :: Capture Int -> Get App ()
handlePut (Capture val) = Get $ do
  logInfo $ "Call put with: " <> show val
  ref <- asks (.current)
  liftIO $ atomicModifyIORef' ref (\cur -> (cur + val, ()))

-- | Helper to do simple logging
logInfo :: String -> App ()
logInfo message = liftIO $ putStrLn $ "[INFO] " <> message
