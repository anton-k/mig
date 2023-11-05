# Using other monads with Server

So far we have seen only `IO` monad to be used with `Server`.
But we can use other monads with it. Although so far
only three types of monads are supported for `Servers`:

* `IO`-monad
* `ReaderT env IO` and `newtype` wrappers on top of it
* `ReaderT env (EitherT IO)` and new type wrappers on top of it

So the library is limited in monad choice but all of the cases
can cover everything you need from the server. 

Also we can use any monad which is convertible to `IO` with function:

```haskell
hoistServer :: forall a . (m a -> n a) -> Server m -> Server n
```

The reason why we would like to convert to `IO` because warp server
convertion function `runServer` works only for the type `Server IO`.
So we can use any monad but we would like to convert to `IO` at the very and
to be able to run our server with warp.

I personally prefer to just use `IO` and pass environment around
to handlers. This process can be automated with `ReaderT` monad.
Let's study how to use `ReaderT` with the server.

As example we will build a server that contains mutable state.
It has internal counter which we can query and increment.
To store the internal state as shared environment for all handlers
we are going to Reader-pattern or server with `ReaderT` over `IO` monad. 

## Reader-pattern

Our server is Json-based but we want custom monad. So we can import the preset module
for `Json`:

```haskell
import Mig.Json
```

Also we import `ReaderT` from `mtl` library and `IORef` to store mutable shared state:

```haskell
import Control.Monad.Reader
import Data.IORef
```

Let's define a type for our application:

```haskell
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
```

### `HasServer` class

We declare it as `newtype`-wrapper with `ReaderT` under the hood.
We can derive all the classes that we need to use it as `Reader`.
All classes but last are common repertoire of the Haskell. 
The last class `HasServer` is special to `mig` library.
It can be also auto-derived as the instance for `ReaderT+IO` is already defined.

The `HasServer` class defines how to convert our special monad m server
to `Server IO`. For a reader it defines a method:

```haskell
  renderServer :: Server (ReaderT env m) -> env -> IO (Server IO)
```

So if we pass the common shared environment `env` to server
we can use it as `Server IO`. We need to convert to `Server IO`
because for WAI and warp we can run only `Server IO` based servers.
As in library `mig-wai`:

```haskell
toApplication :: Server IO -> Wai.Application
```

### How to run Reader based server

So to run the `ReaderT` server we need to convert it to `IO`-based server
and we can run it with usual `runServer` function:


```haskell

main :: IO ()
main = do
  env <- initEnv 
  putStrLn ("The counter server listens on port: " <> show port)
  runServer port $ withSwagger def $ renderServer server env
  where
    port = 8085

server :: Server App
```

Here we also add the swagger to the server for easy testing
and trying things out with swagger.

### Server with Reader monad

Our server has two routes:

* `get` - to query current state
* `put` - to add some integer to the state

```haskell
server :: Server App
server =
  "counter"
    /. [ "get" /. handleGet
       , "put" /. handlePut
       ]
```
Let's define the `get` route:

```haskell
-- | Get handler. It logs the call and returns current state
handleGet :: Get App (Resp Int)
handleGet = Send $ do
  logInfo "Call get"
  ref <- asks (.current)
  liftIO $ ok <$> readIORef ref

-- | Helper to do simple logging
logInfo :: String -> App ()
logInfo message = liftIO $ putStrLn $ "[INFO] " <> message
```

So we ask for the common mutable state and read it with `readIORef` function.
Also we use `liftIO` to lift `IO` result to `App` monad.
We just use `App` monad inside `Send`-wrapper to create a handler.

Let's define the `put` handler:

```haskell
-- | Put handler. It logs the call and updates 
-- the state with integer which is read from URL
handlePut :: Capture "arg" Int -> Post App (Resp ())
handlePut (Capture val) = Send $ do
  logInfo $ "Call put with: " <> show val
  ref <- asks (.current)
  liftIO $ ok <$> atomicModifyIORef' ref (\cur -> (cur + val, ()))
```

We use `atomicModifyIORef'` to be safe in presence of concurrent requests.
So we have completed the definition and we can run the app and try it out.
You can find the complete code of the example in the [`mig` repo](https://github.com/anton-k/mig/blob/main/examples/mig-example-apps/Counter/Main.hs).


## Using custom monad

We have studied how to use `ReaderT IO` and `newtype`-wrappers on top of it
as monads for our server. To use any other monad we need to have the function:

```haskell
runAsIO :: MyMonad a -> IO a
```

For custom monad `MyMonad`. If there is such a function we can use function:

```haskell
hoistServer :: forall a . (m a -> n a) -> Server m -> Server n
```

Prior to call to `runServer` and run the server which is based on our custom monad:

```haskell
main :: IO ()
main = runServer 8085 (hoistServer runAsIO server)

server :: Server MyMonad
server = ...
```

## Summary 

In this chapter we have learned how to use Reader-monad with `mig` library.
We can define our custom wrapper for `ReaderT+IO` and derive instance 
of `HasServer` and we are ready to go.
