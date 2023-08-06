# Mig - library to write composable and lightweight servers

The Mig is a library to build lightweight composable servers.
There are only couple of combinators and Monoid instance.
With it we can build type-safe servers from small parts.

The main strength is ability to build servers from parts
and flexible DSL which features only small amount of functions.

I like [scotty](https://hackage.haskell.org/package/scotty) for being very 
simple and [servant](https://hackage.haskell.org/package/servant-server) for being composable, type-safe 
and how functions are used as handlers which provides decoupling of Web-handlers
from application logic.
But sometimes scotty feels too imperative and lacks servant's composability.
And servant with type-level magic and huge errors can feel to complicated.
So I wanted to create something in the middle. Something composable and simple 
at the same time.
The name `mig` (pronounced as meeg) is a russian word for "instant moment".

## Quick start guide

Let's create something cool with the library.

### Hello world server

As a starting point let's look at hello-world server:

```haskell
module Main where

import Mig
import Data.Text (Text)

main :: IO ()
main = runServer 8080 server

server :: Server IO
server =
  "api" /. "v1" /. "hello" /. hello

hello :: Get Json IO Text
hello = pure "Hello World"
```

The main type is `Server`. We can think about it as function from request to response
which sometimes can produce no output:

```haskell
newtype Server m = Server (Req -> m (Maybe Resp))
```

It is parametrised by the underlying monad. 
So far library supports only three types of monads:
* `IO`-monad
* `ReaderT` over `IO` with possible newtype-wrappers.
* `ReaderT env (ExceptT err) IO` - reader with `ExceptT` over `IO`.

Also we can create our custom monads as newtype wrappers over those monads
and be able to use it with library. To do that we need to derive `HasServer` instance.
It can be done with deriving strategies (see `examples/Counter.hs`). 

To run server we can use functions:

```haskell
-- | Runs server on port
runServer :: Int -> Server IO -> IO ()
runServer port server = ...

-- | Convert to WAI application
toApplication :: ServerConfig -> Server IO -> Wai.Application
```

The HTTP-method is specified with newtype wrapper `Get`:

```haskell
newtype Get ty m a = Get (m a)
```

It has phantom-argument for type of the response. In this example we return `Text`
as response body with 200 ok status. It seems that we need two typed to specify result `ty` and `a`.

Beside `Text` we also can return `Json`, `Html`, raw `ByteString` as response. But we have
several ways to render handler result to response body. For example we can convert Int as Text
and also as JSON. To distinguish between them we use phantom type for the type of response body.

### Using monoid for route branches

The server has Monoid instance. With it we can build
servers from several routes:

```haskell
server :: Server IO
server =
  "api" /. "v1" /.
     mconcat
       [ "hello" /. handle "hello"
       , "bye" /. handle "bye"
       ]

handle :: Text -> Get Text IO Text
handle prefix = Get $ pure $ prefix <> " world"
```

Note how branching by path is done with `Monoid` method `mconcat`.
We use `Get` inside the function `handle`.

### Query parameters 

We can turn a "world" into parameter:

```haskell
server :: Server IO
server =
  "api" /. "v1" /.
     mconcat
       [ "hello" /. handle "hello"
       , "bye" /. handle "bye"
       ]

handle :: Text -> Query "who" Text -> Get Text IO Text
handle prefix (Query name) = Get $ pure $ prefix <> " " <> name
```

By changing the signature of the function we have requested required query
parameter called `"who"`. We use type-level string literals to encode name of the parameter.
It is provided with url: `api/v1/hello?who=john`.

If we use `Optional` instead of `Query` parameter becomes optional and value
is wrapped in `Maybe`.

### Capture URI-parts

In the example we can note the duplication of path name `"hello/bye"` and that we
pass the same constants to our function `handle`.
We can capture that part of URI as argument with `Capture` argument:

```haskell
server :: Server IO
server =
  "api" /. "v1" /. handle

handle :: Capture Text -> Query "who" Text -> Get Text IO Text
handle (Capture prefix) (Query name) = Get $ pure $ prefix <> " " <> name
```

This example is equivalent to previous one. Only we capture part of 
the URI as text and use it in the message. Also with capture we can append all sorts of prefixes.

### Route arguments

The cool part of it is that handle function can have any amount of input arguments
wrapped in special newtypes and it will be decoded to proper server route.

We have newtypes for:

* `Query "name" type` - required query parameter
* `Optional "name" type` - optional query parameter
* `Capture type` - capture part of the URI between slashes `/`.
* `Body type` - input JSON body
* `RawBody` - input body as raw lazy bytestring
* `RawFormData` - input of the html-form
* `FormJson` - input f html-form as Json (see examples/Html.hs)
* `Header "name"` - access header by name
* `PathInfo` - access path info relative to the server

We can change the number of arguments because the function `(/.)` is overloaded
by second argument and it can accept anything convertible to `Server` or an
instance of the class `ToServer`.

### Route outputs

Also `newtype` wrappers can control behavior of the output.
We already saw `Get`-wrapper. It encodes Http-method. Also we can use
`Post`, `Put`, `Delete`, etc.

We have output wrappers for:

* http-methods: `Get`, `Post`, `Put`, `Delete`, etc.
* append headers: `AddHeaders a`
* change response status: `SetStatus a`
* return error: `Either (Error ty) a`

We can nest wrappers to apply several behaviors. 
For example we can update header, possible return error and return Post-method:

```haskell
handle :: Query "foo" Int -> Post Json IO (Either (Error Text) (AddHeaders FooResponse))
```

Here `FooResponse` should have `ToJSON` instance. Possible implementation:

```haskell
data FooResponse = FooResponse
  { code :: Int
  , message :: Text
  }
  deriving (Generic, ToJSON)

handle (Query code) = Post $ do
  message <- readMessageBycode code
  pure $ Right $ AddHeaders headers $ FooResponse code message
  where
    headers = ["Trace-Code", Nothing]
```

### Errors

The errors can be returned from route with `(Either (Error ty))` output wrapper.
We signify to the user that our route returns errors.
The `Error` type contains status and details for the error:

```haskell
data Error a = Error
  { status :: Status
    -- error status
  , body :: a
    -- message or error details
  }
```

Note that `ToServer` instance takes care about proper conversion of the error
value to the same response type as the main happy route branch.

## Specific servers

If we write server of specific type. For example if we write JSON API with IO-based server
we can import specific route newtype-wrappers:

```haskell
import Mig.Json.IO
```

It will simplify the signatures of the functions:

```haskell
handle :: Body FooRequest -> Post FooResponse
handle (Body req) = Post $ do
  resp <- readResp req
  pure resp
```

As `Post` becomes specified to `Json` and `IO`:

```haskell
newtype Post a = Post (IO a)
```

There are similar modules for `Html`. If your server is not `IO`-based
Use import of `Mig.Json`.

## Reader based servers

There is very popular pattern of writing servers with monad `ReaderT ServerContext IO`.
The server context can contain shared context of the server and mutable stated wrapped in `TVar`'s
or IO-based interfaces. We can access the context inside handler and shared for all routes.

The `mig` has support for Reader-pattern like monads.
Let's build a simple counter server as example. User can see current value with `get` and add 
to the internal counter with method `put`.

Let's define application monad first

```haskell
newtype App a = App (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, HasServer)

data Env = Env
  { current :: IORef Int
  }
```

Note the deriving of `HasServer`. It is defined for reader over IO.
With it we can convert the `Server App` to `IO`-based server:

```haskell
renderServer :: Server App -> Env -> IO (Server IO)
```

So we can define our handlers with App-monad and render to `IO` to convert it to WAI-application
and run as server.

Let's define the server:

```haskell
counter :: Server App
counter = do
  "counter" /. "api" /.
    mconcat
      [ "get" /. handleGet
      , "put" /. handlePut
      ]

handleGet :: Get Text App Int
handelGet = -- todo

handlePut :: Capture Int -> Post Text App ()
handlePut (Capture val) = -- todo
```

We can render the server and run it:

```haskell
main :: IO ()
main = do
  env <- initEnv
  server <- renderServer counter env
  runServer 8085 server
```

Let's define the missing parts:

```haskell
initEnv :: IO Env
initEnv = Env <$> newIORef 0

handleGet :: Get Text App Int
handleGet = Get $ do
  ref <- asks (.current)
  liftIO $ readIORef ref

handlePut :: Capture Int -> Get Json App ()
handlePut (Capture val) = Get $ do
  ref <- asks (.current)
  liftIO $ atomicModifyIORef' ref (\cur -> (cur + val, ()))
```

So we have studied how we can use custom Reader-based monads.
The trick is to derive `HasServer` on newtype wrapper and 
use method `renderServer` to convert to `IO`-based server.

PS: this is an open question. Is it possible to create a function:

```haskell
hoistServer :: (Monad m, Monad n) => (forall a . m a -> n a) -> Server m -> Server n
```

As it is defined in the servant. With it we would be able to use any monad.
But I'm not sure how to achieve that. Help is appreciated, as it will make library even better!
I guess it can be done with `MonadBaseControl` and if we turn the WAI function to:

```haskell
toApplication :: MonadBaseControl m => Server m -> m Wai.Application
```

## Conclusion

We have walked through the whole library. As a summary of it's functions: we can 

* compose servers with path operator `(/.)` and monoid instance. 
* define handlers as functions with various input and output newtype-wrappers

I hope that you like the concept and will enjoy the library. See the directory [`examples`](https://github.com/anton-k/mig/tree/main/examples) for more examples.
We can run the examples with stack by running:

```
> make run
```
in this repo. Change the Makefile to try different examples.


Also there are repos that show how to use library with most common
Haskell patterns to create web-servers:

* [Handle pattern](https://github.com/anton-k/handle-pattern-mig-app)
* [Reader patten](https://github.com/anton-k/reader-pattern-mig-app)

This is a very first sketch of the library. I guess it can become even better. 
The feedback is appreciated.

