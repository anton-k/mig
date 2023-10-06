# Hello world

Let's build hello world application. 
We are going to build simple JSON API server with single route which replies
with constant text to request

We have installed the library `mig-server`. Let's import the main module.
It brings into the scope all main functions of the library:

```haskell
module Main where

import Mig
```

Let's define a server with single route:

```haskell
server :: Server IO
server = "api/v1/hello" /. hello

hello :: Get IO (Resp Json Text)
hello = undefined
```

So we serve single route with path `"api/v1/hello"`.
Let's cover the types first.

### The server type

The server is a description of both OpenAPI schema for our server and low-level function
to run it. In the library it is a newtype wrapper:

```haskell
newtype Server m = Server (Api (Route m))
```

The `Api` type is a value to describe the API schema and `Route` contains
useful info on the type of the route (method, decription of the inputs and outputs).
The server is parametrized by some monad type. For this example we use `IO`-monad.
It means that all our hadnlers are going to return `IO`-values.

### How to link paths to handlers

To bind path "api/v1/hello" to handler `hello` we use function `(/.)`. Let's look at it's type signature:

```haskell
(/.) :: (ToServer a) => Path -> a -> Server (MonadOf a)
```

It expects the `Path` which has instance of class `IsString` that is why we can
use plain strings for it. the second argument is something that is convertible to `Server`.
Here we use trick to be able to use arbitrary haskell functions as handlers.
We have special class called `ToServer` which can convert many different types to `Server`.

The output type is abit tricky: `Server (MonadOf a)`.
The `MonadOf` is a type function which can extract `m` from `(Server m)`.
Or for example it can extract `m` from the function `request -> m response`.
So the `MonadOf` is a way to get underlying server monad from any value.

Let's be more specific and study our example. 
The type of the handler is `Get IO (Resp Text)`
In our case we get:

```
(/.) :: Path -> Get IO (Resp Text) -> Server IO
```

The type-level function `MonadOf` knows how to extract `IO` from `Get IO (Resp Text)`.

### The type of response

Let's stydy the signature of the `hello` handler: 

```
hello :: Get IO (Resp Json Text)
          |  |    |     |    |
          |  |    |     |    +-- response body converted to byte string
          |  |    |     |
          |  |    |     +---- codec to convert it 
          |  |    |           (the media-type route uses for response body)
          |  |    |
          |  |    +---- type of response which holds HTTP-response info with result
          |  |
          |  +----- the server monad. Our handler returns values in this monad
          |
          +----- http method encoded as a type
```       

The type `Get` is a synonym for more generic `Send` type:

```haskell
type Get m a = Send GET m a
```

The type `Send` is just a wrapper on top of monadic value:

```haskell
newtype Send method m a = Send (m a)
```

It encodes HTTP-method on type level. This is useful to aggregate value for API-schema of our server.
We have type synonyms for all HTTP-nethods (`Get`, `Post`, `Put` etc).

It's interesting to know that library mig does not use any custom monads for operation. 
Instead it runs on top of monad provided by the user. Usually it would be `IO` or `Reader` over `IO`.

### HTTP-response type

Let's study the `Resp` type. It is a type for HTTP response.
It contains the value and additional HTTP information:

```haskell
-- | Response with info on the media-type encoded as type.
data Resp media a = Resp
  { status :: Status
  -- ^ response status
  , headers :: ResponseHeaders
  -- ^ response headers
  , body :: Maybe a
  -- ^ response body. Nothing means "no content" in the body
  }
  deriving (Show, Functor)
```

The type argument `media` is interesting. It gives a hint to the compiler on how
to convert the body to low-level byte string representation.
In our example we use type-level tag `Json` to show that we are going to convert
the result to JSON value in the response. So in our case of `Resp Json Text`
we are going to return `Text` which will be converted to JSON value.

To return successful response there is  a handy function:

```haskell
ok :: a -> Resp media a
```

It returns response with 200 ok-status and sets `Content-Type` header to proper media-type.

### Define a handler

Let's complete the example and define a handler which returns static text:

```haskell
hello :: Get IO (Resp Json)
hello = Send $ pure $ ok "Hello World!"
```

We have several wrappers here:

* `ok` - converts text value to http-response `Resp Json Text`
* `pure` - converts pure value  to IO-based value
* `Send` - send converts monadic value to server. It adds information on HTTP-method of the return type.

### Run a server

Let's run the server with warp. For that we define the `main` function for our application:

```haskell
main :: IO ()
main = do
  putStrLn $ "Server starts on port: " <> show port
  runServer port server
  where
    port = 8085
```

That's it! We can compile the code and run it to query our server.
We use the function `runServer`:

```haskell
runServer :: Int -> Server IO -> IO ()
```

It renders our server to WAI-application and runs it with warp.

### Complete code for the example


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

hello :: Get IO (Resp Json)
hello = Send $ pure $ ok "Hello World!"
```

If we run the code we can test it with `curl` in command line:

```
> curl http://localhost:8085/api/v1/hello

"Hello World!"
```

### Add more routes

Let's define another handler to say `bye`:

```haskell
bye :: Get IO (Resp Json)
bye = Send $ pure $ ok "Goodbye"
```

We can add it to the server with monoid method as `Server m` is a `Monoid`:

```haskell
server :: Server IO
server = 
  "api/v1" /.
    mconcat
      [ "hello" /. hello
      , "bye" /. bye
      ]
```

The meaning of the monoid methods for `Server`:

* `mempty` - server that always fails on any request
* `a <> b` - try to serve the request with server `a` if it succeeds return the result.
   If it fails try to serve with server `b`.

So we have just two functions to build nested trees of servers:

* `path /. server` - to serve the server on specific path
* `mconcat [a, b, c, d]` - to combine several servers into one

Note that we can have several handlers on the same path if they
have different methods or media-types for output or input:

```haskell
server = 
  "api/v1" /.
    mconcat
      [ "hello" /. helloGet
      , "hello" /. helloPost
      ]

helloGet :: Get IO (Resp Json Text)
helloPost :: Post IO (Resp Json Text)
```

Servers on the same path are also distinguished by:

* http-method
* media-type of the result (value of "Accept" header) 
* media-type of the request (value of "Content-Type" header)

### Subtle nuance on Monoid instance for Server

Yuo may ask: why not to write the previous example like this:

```haskell
server = 
  "api/v1/hello" /.
    mconcat
      [ helloGet
      , helloPost
      ]
```

There is  a subtle nuance here. The `Server m` is a `Monoid`.
But the value `Send method m a` is not. So we use the function `(/.)`
which converts the second argument to `Server`. If we want to convert
we can use the method of the class `ToServer`:

```haskell
toServer :: ToServer a => a -> Server (MonadOf a)
```

So the right way to avoid duplication in path is:

```haskell
server = 
  "api/v1/hello" /.
    mconcat
      [ toServer helloGet
      , toServer helloPost
      ]
```

Regarding the previous example we could not use `mconcat` even if we wnated to.
Because `handelGet` and `handlePost` have different types. They can not
be even put in the same list. But here lies the beauty of the library.
We can use arbitrary types as handlers but in the end they all get converted
to the value `Server m`. So we have the flexibility on DSL level but
on the level of implementation to build the tree of handlers we use the same type.
which makes type very simple.

### The path type

Let's discuss the `Path` type.
It is a list of atomic path items:

```haskell
newtype Path = Path [PathItem]
  deriving (Show, Eq, Semigroup, Monoid)
```

The path item can be of two types:

```haskell
data PathItem 
  = StaticPath Text
  | CapturePath Text
```

The static path item is a rigid entity with exact match to string.
We used it in all our examples so far. 
but capture is wild-card that is going to be used as input to the handler.

To construct only rigid paths we can use strings:

```
"ap1/v1/get/blog/post"
"foo/bar"
```

To get captures we use `*`-wildcard:

```
api/v2/*/get
```

In the star request captures any text. There might be as many stars 
in the path as you wish. But they should be supported by the handler. 
We will touch upon that later.

It's good to know that path is a special type which can be constructed from strings
(use `OverloadedStrings` extension). And we can two types of atomic path elements.
Static items and capture parameters. We will deal with captures in the next example.
