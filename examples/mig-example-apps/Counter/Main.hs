{-# LANGUAGE UndecidableInstances #-}

--  | Example shows how to use custom monad with server.
-- We use Json response server with our Monad App which is a newtype over ReaderT-IO.
-- As we can run only @Server IO@ we need to convert the @Server App@ to IO-based server.
-- We can do it with @renderServer@ function from the class @HasServer@.
--
-- We need to derive the class @HasServer@ for our monad App. It can be done easily with @DerivingStrategies@
-- extension as our monad is a newtype wrapper over ReaderT-IO and for that monad server can be derived.
--
-- Also we can derive instance for the newtypes over @ReaderT env (ExceptT err IO)@
module Main (
  main,
) where

import Control.Monad.Reader
import Data.IORef
import Mig.Json

main :: IO ()
main = do
  env <- initEnv
  putStrLn ("The counter server listens on port: " <> show port)
  runServer port . withSwagger def $ renderServer server env
  where
    port = 8085

-------------------------------------------------------------------------------------
-- server types

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

-- | Init shared state
initEnv :: IO Env
initEnv = Env <$> newIORef 0

-------------------------------------------------------------------------------------
-- server definition

{-| Server has two routes:

* get - to querry current state
* put - to add some integer to the state
-}
server :: Server App
server =
  "counter"
    /. [ "get" /. handleGet
       , "put" /. handlePut
       ]

-- | Get handler. It logs the call and returns current state
handleGet :: Get App (Resp Int)
handleGet = Send $ do
  logInfo "Call get"
  ref <- asks (.current)
  liftIO $ ok <$> readIORef ref

-- | Put handler. It logs the call and updates the state with integer which is read from URL
handlePut :: Capture "arg" Int -> Get App (Resp ())
handlePut (Capture val) = Send $ do
  logInfo $ "Call put with: " <> show val
  ref <- asks (.current)
  liftIO $ ok <$> atomicModifyIORef' ref (\cur -> (cur + val, ()))

-- | Helper to do simple logging
logInfo :: String -> App ()
logInfo message = liftIO $ putStrLn $ "[INFO] " <> message
