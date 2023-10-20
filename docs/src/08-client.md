# Clients

We can define HTTP-clients from the same definition as servers.
Let's start with hello world example.

The server code is:

```haskell
module Main (main) where

import Mig

main :: IO ()
main = do
  putStrLn $ "Server starts on port: " <> show port
  runServer port server
  where
    port = 8085

server :: Server IO
server = "api/v1/hello" /. hello

hello :: Get IO (Resp Json Text)
hello = Send $ pure $ ok "Hello World!"
```

To turn that into server we use the class `ToClient`:


```haskell
class ToClient a where
   -- | converts to client function
   toClient :: Server m -> a

   -- | how many routes client has
   clientArity :: Int
```

It can convert the `Server` definition to a client.
The client relies on special monad `Client`, which we can
run with function:


```haskell
runClient :: ClientConfig -> Client a -> IO (RespOr AnyMedia BL.ByteString a)

-- | Config to run the clients
data ClientConfig = ClientConfig
  { port :: Int
  -- ^ port to connect to
  , manager :: Http.Manager
  -- ^ HTTP-manager
  }
```

To share code between server and client we should slightly modify our server
code and introduce a type synonym for the type of the route:

```haskell
type Hello m = Get m (Resp Json Text)

server :: Hello m -> Server m
server handler = "api/v1/hello" /. handler

hello :: Hello IO
hello = Send $ pure $ ok "Hello World!"
```

Our server also becomes generic in it's monad type. If we want to create a server
we can apply the handler function to server:

```haskell
helloServer :: Server IO
helloServer = (server hello)
```

To define a client we use monad `Client` as parameter and get server 
definition with `toClient` method:

```haskell
helloClient :: Hello Client
helloClient = server helloClient
```

We need to provide some argument to the server function and
here we use recursive definition we pass the result back to the argument.
This code is ok because we do not need the implementation of the server
in the `toClient` function. We rely solely on the type signature and path
to the handler. Which is specified in the `server` function the route handler
is never touched by the execution path. So it is ok to use recursive definition.
We can also pass `undefined`.

After that we can call a client funtion:

```haskell
import Data.ByteString.Lazy qualified as BL
import Mig
import Mig.Client
import Network.HTTP.Client qualified as Http -- from http-client library

main :: IO ()
main = do
  config <- ClientConfig port <$> Http.newManager Http.defaultManagerSettings
  print =<< runHello config
  where
    port = 8085

-- | Make it convenient to use
runHello :: ClientConfig -> IO (Either BL.ByteString Text)
runHello config = getRespOrValue <$> runClient config hello
```

We use function to get result:

```haskell
getRespOrValue :: RespOr media BL.ByteString a -> Either BL.ByteString a
```

## Several routes

If we have several routes we can use tuples in the result or special
combinator `:|`. Let's create a client for Counter example. Let's recall how server is defined:

```haskell
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


handleGet :: Get App (Resp Int)
handlePut :: Capture "arg" Int -> Get App (Resp ())
```

Let's parametrize by the monad type to share the code:

```haskell
-- | Routes for the server
data Routes m = Routes
  { get :: Get m (Resp Int)
  , put :: Capture "args" Int -> Get m (Resp ())
  }

server :: Routes m -> Server m
server routes =
  "counter"
    /. [ "get" /. routes.get
       , "put" /. routes.put
       ]
```

We can define a server by applying routes:

```haskell
counterServer :: Server IO
counterServer = server (Routes handleGet handlePut)
```

To create client we use `ToClient` class again:

```haskell
counterClient :: Routes Client
counterClient = Routes getClient putClient
  where
    getClient :| putClient = toClient (server counterClient)
```

We use recursive definition to tie the knot and provide
dummy functions to get the server.

The `:|` combinator is just a convenient synonym for pair `(,)`.
It can be handy if we have arbitrary number of the routes:

```haskell
routeA
:| routeB
:| routeC
:| routeD
... = toClient
```

Here we rely on the haskell ability to pattern match on the constructors.
In haskell we can use not only single variables on the left side but also 
we can use a constructor. So this is a valid haskell expression:

```haskell
ints :: [Int]
strings :: [String]

(ints, strings) = unzip [(1,"a"), (2, "b")]
```

The same trick is used to specify several routes at once:

```haskell
  where
    getClient :| putClient = toClient (server counterClient)
```

The method `toClient` by the output type knows how many routes
to fetch from server definition with the help of `clientArity` method.
Also we can write this definition with just a tuple:

```haskell
  where
    (getClient, putClient) = toClient (server counterClient)
```

## FromClient class

Often we do not need the request input wrappers on the level of Http-client.
Ww would like to get the function:

```haskell
putClient :: Int -> IO (Either ByteString ())
```

instead of 

```haskell
putClient :: Capture "arg" Int -> IO (Either ByteString ())
```

For that we have a class which can strip away all newtype wrappers:

```haskell
class FromClient a where
  type ClientResult a :: Type
  fromClient :: a -> ClientResult a
```

Associated class `ClientResult` can map from wrapped version to unwrapped one.
We can use it simplify client functions after transformation:

```haskell
-- defined in the library
newtype Client' a = Client' (ReaderT ClientConfig IO a)

type ClientOr a = Client' (Either BL.ByteString a)

-- our client functions:

runGet :: ClientOr Int
runGet = getRespOrValue <$> fromClient getClient

runPut :: Int -> ClientOr ()
runPut = getRespOrValue <$> fromClient putClient
```

We use special variant of client monad which encapsulates `ClientConfig` in the reader.
We can run the `client'` with function:

```haskell
runClient' :: ClientConfig -> Client' a -> IO a
```

So with `FromClass` we can unwrap all newtype arguments from wrappers
and get ClientConfig encapsulated in the reader monad.

## The structure of the client

So let's recap. To define a client we make server definition generic
in the underlying monad and usually introduce a data structure for the routes:

```haskell
data Routes m = Routes
  { auth :: Auth m
  , getBlogPost :: GetBlogPost m
  , writeBlogPost :: WriteBlogPost m
  }

server :: Routes m -> Server m
server routes = ...
```

Also it's good to create the type synonyms for routes so that
we do not need to retype them twice for servers and clients.

After that we can use `ToClient` class to convert server to client:

```haskell
appClient :: Routes Client
appClient = Routes {..}
  auth :| getBlogPost :| writeBlogPost = toClient $ toClient appClient
```

When we got the client we can simplify definition 
by stripping away all newtype-wrappers:

```haskell
runAuth :: Arg1 -> Arg2 -> ClientOr AuthId
runAuth arg1 arg2 = getRespOrValue <$> fromClient appClient.auth arg1 arg2

runGetBlogPost :: Arg1 -> ClientOr BlogPost
runGetBlogPost arg1 = getRespOrValue <$> fromClient appClient.getBlogPost arg1

...
```

You can find examples of the clients in the `examples` directory of the mig repo.



