{-# LANGUAGE UndecidableInstances #-}

module Server (
  counterServer,
  initEnv,
  App,
  Env,
) where

import Api
import Control.Monad.Reader
import Data.IORef
import Mig.Json

{-| Custom type for application monad which is based on Reader-IO pattern.
Note the HasServer instance. It allows us to render server to IO-based one
which we can run as warp + WAI server
-}
newtype App a = App (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, HasServer)

{-| Common shared state
We can put more shared state if we need. Like logger state or some interfaces.
-}
data Env = Env
  { current :: IORef Int
  }

counterServer :: Server App
counterServer = server (Routes handleGet handlePut)

-- | Init shared state
initEnv :: IO Env
initEnv = Env <$> newIORef 0

-- | Get handler. It logs the call and returns current state
handleGet :: GetCounter App
handleGet = Send $ do
  logInfo "Call get"
  ref <- asks (.current)
  liftIO $ ok <$> readIORef ref

-- | Put handler. It logs the call and updates the state with integer which is read from URL
handlePut :: PutCounter App
handlePut (Capture val) = Send $ do
  logInfo $ "Call put with: " <> show val
  ref <- asks (.current)
  liftIO $ ok <$> atomicModifyIORef' ref (\cur -> (cur + val, ()))

-- | Helper to do simple logging
logInfo :: String -> App ()
logInfo message = liftIO $ putStrLn $ "[INFO] " <> message
